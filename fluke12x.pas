unit fluke12x;

interface

uses
  serialutils, classes, sysutils, types;

type
  TTraceAdmin = record
    block_header: byte;     // 128 - admin header only, 0 = admin header and samples  requested
    block_length: uint16;
    trace_process: byte;    // 1=normal, 2=averaged values, 3=envelope
    trace_result: byte;     // 1=trace acquisition, 2=trace plot, 3=hold trace
    misc_setup: byte;       // Coupling mode of signal - bit 7: 0=AC, 1=DC
    y_unit, x_unit: byte;   // 1=V, 2=A, 3=ohm, 5=F, 7=s, 10=Hz, etc.
    y_zero, x_zero,
    y_res, x_res: Double;
    DateTime: TDateTime;
    checksum: byte;
  end;

  TTraceData = record
    block_header: byte;     // 128 - admin header only, 0 = admin header and samples requested, or 129
    block_length: uint16;
    Signed: boolean;
    MinMaxPairs: boolean;
    BytesPerSample: integer;
    overload_value: integer;
    underload_value: integer;
    invalid_value: integer;
    numSamples: uint16;
    checksum: byte;
  end;
  TLogMethod = procedure(const s: string) of object;

  { TFluke12X }

  TFluke12X = class
  private
    FSerial: TSerialObj;
    FLog: TLogMethod;
    procedure Log(const s: string);
    function threeByteFloat(b1, b2, b3: byte): double;
    // Check acknowledge and remove from string
    function checkError(var s: string): boolean;
    // Check acknowledge only
    function checkError(const b: TBytes): boolean;
  public
    destructor Destroy; override;
    function connectFluke(serialPort: string; out connectString: string): boolean;
    // Execute a command, only wait for acknowledge doesn't wait for return data
    function executeCmd(cmd: string; timeoutms: integer = 200): boolean;
    // Execute query and wait for return data
    function executeQuery(cmd: string; out response: string; timeoutms: integer = 200): boolean;
    function screenGrabAsPS(var PS: TStrings): boolean;
    function setBaud19200: boolean;
    function traceGrab(cmd: string; out traceAdmin: TTraceAdmin; out
      traceDataDesc: TTraceData; out data: TIntegerDynArray): boolean;
    property DoLog: TLogMethod read FLog write FLog;
  end;

const
  cmdGetID = 'ID'#13;
  cmdBaud1200 = 'PC 1200'#13;
  cmdBaud19200 = 'PC 19200'#13;
  cmdPrintPS = 'QP 0,3'#13;
  cmdPrintPNG = 'QP 0,11,B'#13;

  // V = values, S = setup info, nothing = setup info + sample
  cmdTraceGrabA_setup_data = 'QW 11'#13;
  cmdTraceGrabB_setup_data = 'QW 21'#13;
  cmdTraceGrabA_setup = 'QW 11,S'#13;
  cmdTraceGrabB_setup = 'QW 21,S'#13;
  cmdTraceGrabA_data = 'QW 11,V'#13;
  cmdTraceGrabB_data = 'QW 21,V'#13;

  flukeTraceProcess: array[0..3] of string = ('', 'Normal', 'Average', 'Envelope');
  flukeTraceResult: array[0..3] of string = ('', 'Acquisition', 'TrendPlot', 'TouchHold');
  flukeMiscSetup: array[0..1] of string = ('AC', 'DC');
  flukeUnits: array[0..18] of string = ('', 'V', 'A', 'Ohm', '', 'F', '', 's', '', '', 'Hz', 'deg',
                'degC', 'degF', '%', 'dbm50', 'dbm600', 'dbv', 'dba');

implementation

uses
  math, dateutils;

{ TFluke12X }

procedure TFluke12X.Log(const s: string);
begin
  if Assigned(FLog) then
    FLog(s);
end;

function TFluke12X.threeByteFloat(b1, b2, b3: byte): double;
var
  m: int16;
begin
  if b1 < 128 then
    m := b1 * 256 + b2
  else
    m := -(256 - b1) * 256 + b2;

  result := m * intpower(10, Int8(b3));
end;

function TFluke12X.checkError(var s: string): boolean;
begin
  result := false;
  case s[1] of
    '0': result := true;
    '1': Log('Syntax error');
    '2': Log('Execution error');
    '3': Log('Synchronization error');
    '4': Log('Communications error');
    '5': Log('No data available')
    else Log('Unknown acknowledge');
  end;
  s := Copy(s, 3, length(s));
end;

function TFluke12X.checkError(const b: TBytes): boolean;
begin
  result := false;
  case char(b[0]) of
    '0': result := true;
    '1': Log('Syntax error');
    '2': Log('Execution error');
    '3': Log('Synchronization error');
    '4': Log('Communications error');
    '5': Log('No data available')
    else Log('Unknown acknowledge');
  end;
end;

destructor TFluke12X.Destroy;
begin
  if Assigned(FSerial) then
    FreeAndNil(FSerial);
end;

function TFluke12X.connectFluke(serialPort: string; out connectString: string
  ): boolean;
var
  s: string;
begin
  connectString := 'Failed to connect';
  if not Assigned(FSerial) then
    FSerial := TSerialObj.Create;

  result := fSerial.OpenPort(serialPort, 1200);
  if result then
  begin
    Log('Connected...');

    result := setBaud19200;
    if result then
    begin
      if executeQuery(cmdGetID, s) then
      begin
        Log(s);
        connectString := s;
      end;
    end;
  end
  else
    Log('Error connecting to serial...');
end;

function TFluke12X.executeCmd(cmd: string; timeoutms: integer): boolean;
const
  abortAfterRetries = 3;
var
  b: TBytes;
  len, i, noReplyCount: integer;
begin
  result := false;
  if fSerial.Write(cmd[1], length(cmd)) <> length(cmd) then
    Log('Error sending cmd '+cmd)
  else
  begin
    setlength(b, 2); // only read response byte and <cr>
    i := 0;  // offset for new data in buffer
    noReplyCount := 0;
    repeat
      len := fSerial.ReadTimeout(b[i], length(b)-i, timeoutms);
      i := i + len;
      if len = 0 then
        inc(noReplyCount);
    until (i > 1) or (noReplyCount > abortAfterRetries);

    if i > 1 then
      result := checkError(b);
  end;
end;

function TFluke12X.executeQuery(cmd: string; out response: string;
  timeoutms: integer): boolean;
var
  b: TBytes;
  len, i: integer;
begin
  result := false;
  if fSerial.Write(cmd[1], length(cmd)) <> length(cmd) then
    Log('Error sending cmd '+cmd)
  else
  begin
    setlength(b, 2048);
    setlength(response, 0);
    i := 1;  // pointer to current data end in s
    repeat
      len := fSerial.ReadTimeout(b[0], length(b), timeoutms);
      if len > 0 then // append to buf
      begin
        setlength(response, length(response) + length(b));
        move(b[0], response[i], length(b));
        inc(i, length(b));
      end;
    until (len < 1);

    if i > 1 then
    begin
      result := checkError(response);
    end;
  end;
end;

function TFluke12X.screenGrabAsPS(var PS: TStrings): boolean;
var
  s: string;
begin
  result := executeQuery(cmdPrintPS, s, 5000);
  if result then
    PS.Text := s
  else
    PS.Text := '';
end;

function TFluke12X.setBaud19200: boolean;
begin
  result := false;
  if executeCmd(cmdBaud19200, 1000) then
  begin
    if fSerial.OpenPort(FSerial.portName, 19200) then
    begin
      Log('PC serial baud set to 19200');
      result := true;
    end
    else
      Log('Error setting serial baud to 19200 on PC');
  end
  else
    Log('Error setting Fluke baud to 19200');
end;

function TFluke12X.traceGrab(cmd: string; out traceAdmin: TTraceAdmin;
  out traceDataDesc: TTraceData; out data: TIntegerDynArray): boolean;
var
  s, s1: string;
  i, j, century, month, day, hour, minute, second: integer;
  cs: byte;
begin
  result := false;
  if executeQuery(cmd, s, 5000) then
  begin
    Log('Response length = ' + IntToStr(length(s)));

    if (s[1] = '#') and (s[2] = '0') then  // trace_admin starts with #0
    begin
      traceadmin.block_header := ord(s[3]);
      traceadmin.block_length := ord(s[4])*256 + ord(s[5]);
      traceadmin.trace_process := ord(s[6]);
      traceadmin.trace_result := ord(s[7]);
      traceadmin.misc_setup := ord(s[8]);
      traceadmin.y_unit := ord(s[9]);
      traceadmin.x_unit := ord(s[10]);
      traceadmin.y_zero := threeByteFloat(ord(s[11]), ord(s[12]), ord(s[13]));
      traceadmin.x_zero := threeByteFloat(ord(s[14]), ord(s[15]), ord(s[16]));
      traceadmin.y_res := threeByteFloat(ord(s[17]), ord(s[18]), ord(s[19]));
      traceadmin.x_res := threeByteFloat(ord(s[20]), ord(s[21]), ord(s[22]));

      s1 := s[23]+s[24]+s[25]+s[26];
      century := StrToInt(s1);
      s1 := s[27]+s[28];
      month := StrToInt(s1);
      s1 := s[29]+s[30];
      day := StrToInt(s1);
      s1 := s[31] + s[32];
      hour := StrToInt(s1);
      s1 := s[33] + s[34];
      minute := StrToInt(s1);
      s1 := s[35] + s[36];
      second := StrToInt(s1);
      traceadmin.DateTime := EncodeDateTime(century, month, day, hour, minute, second, 0);

      //traceadmin.time_stamp := s[31]+s[32]+':'+s[33]+s[34]+':'+s[35]+s[36];
      traceadmin.checksum := ord(s[37]);

      Log('<block_header> = ' + IntToStr(traceadmin.block_header));
      Log('<block_length> = ' + IntToStr(traceadmin.block_length));
      Log(format('<trace_process> = %d [%s]', [traceadmin.trace_process, flukeTraceProcess[traceadmin.trace_process]]));
      Log(format('<trace_result> = %d [%s]', [traceadmin.trace_result, flukeTraceResult[traceadmin.trace_result]]));
      Log(format('<misc_setup = %d [%s]', [traceadmin.misc_setup, flukeMiscSetup[traceadmin.misc_setup shr 7]]));
      Log(format('<y_unit> = %d [%s]', [traceadmin.y_unit, flukeUnits[traceadmin.y_unit]]));
      Log(format('<x_unit> = %d [%s]', [traceadmin.x_unit, flukeUnits[traceadmin.x_unit]]));
      Log('<y_zero> = ' + FloatToStr(traceadmin.y_zero));
      Log('<x_zero> = ' + FloatToStr(traceadmin.x_zero));
      Log('<y_res> = ' + FloatToStr(traceadmin.y_res));
      Log('<x_res> = ' + FloatToStr(traceadmin.x_res));
      Log('<date_stamp = ' + DateToStr(traceadmin.DateTime));
      Log('<time_stamp = ' + TimeToStr(traceadmin.DateTime));
      Log('<checksum> = ' + IntToStr(traceadmin.checksum));

      cs := 0;
      for i := 0 to traceadmin.block_length-1 do
        cs := byte(byte(cs) + byte(s[6+i]));

      if cs = traceadmin.checksum then
        Log('Checksum OK')
      else
        Log(format('Checksum error: %d <> %d', [traceadmin.checksum, cs]));

      i := 7 + traceadmin.block_length;
      if s[i] = ',' then
      begin
        if (s[i+1] = '#') and (s[i+2] = '0') then
        begin
          traceDataDesc.block_header := ord(s[i+3]);
          traceDataDesc.block_length := ord(s[i+4])*256 + ord(s[i+5]);
          traceDataDesc.BytesPerSample := ord(s[i+6]) and 7;
          traceDataDesc.MinMaxPairs := (ord(s[i+6]) and 64) <> 0;
          traceDataDesc.Signed := (ord(s[i+6]) and 128) <> 0;
          cs := ord(s[i+6]);
          if traceDataDesc.BytesPerSample = 2 then
          begin
            traceDataDesc.overload_value := int16(ord(s[i+7])*256 + ord(s[i+8]));
            traceDataDesc.underload_value := int16(ord(s[i+9])*256 + ord(s[i+10]));
            traceDataDesc.invalid_value := int16(ord(s[i+11])*256 + ord(s[i+12]));
            traceDataDesc.numSamples := ord(s[i+13])*256 + ord(s[i+14]);
            cs := byte(cs + ord(s[i+7]) + ord(s[i+8]) + ord(s[i+9]) +
                       ord(s[i+10]) + ord(s[i+11]) + ord(s[i+12]) + ord(s[i+13]) + ord(s[i+14]));
            i := i + 15;
          end
          else
          begin
            traceDataDesc.overload_value := int8(ord(s[i+7]));
            traceDataDesc.underload_value := int8(ord(s[i+8]));
            traceDataDesc.invalid_value := int8(ord(s[i+9]));
            traceDataDesc.numSamples := ord(s[i+10])*256 + ord(s[i+11]);
            cs := byte(cs + ord(s[i+7]) + ord(s[i+8]) + ord(s[i+9]) +
                       ord(s[i+10]) + ord(s[i+11]));
            i := i + 12;
          end;

          SetLength(data, traceDataDesc.numSamples);
          j := 0;
          while j < traceDataDesc.numSamples do
          begin
            if traceDataDesc.BytesPerSample = 2 then
            begin
              data[j] := int16(ord(s[i + 2*j])*256 + ord(s[i + 2*j + 1]));
              cs := byte(cs + ord(s[i + 2*j]) + ord(s[i + 2*j + 1]));
            end
            else
            begin
              data[j] := int8(ord(s[i + j]));
              cs := byte(cs + ord(s[i + j]));
            end;
            inc(j);
          end;

          i := i + traceDataDesc.BytesPerSample*traceDataDesc.numSamples;

          // Now copy checksum at end
          traceDataDesc.checksum := ord(s[i]);
          if s[i+1] <> #13 then
            Log('Unexpected termination char: ' + s[i+1]);

          Log(#10'Data header:');
          Log('<block_header> = ' + IntToStr(traceDataDesc.block_header));
          Log('<block_length> = ' + IntToStr(traceDataDesc.block_length));
          Log(format('<sample_format> = signed=%s, minmax=%s, bytes/sample=%d',
                     [BoolToStr(traceDataDesc.Signed), BoolToStr(traceDataDesc.MinMaxPairs), traceDataDesc.BytesPerSample]));
          Log('<overload_value> = ' + IntToStr(traceDataDesc.overload_value));
          Log('<underload_value> = ' + IntToStr(traceDataDesc.underload_value));
          Log('<invalid_value> = ' + IntToStr(traceDataDesc.invalid_value));
          Log('<numSamples> = ' + IntToStr(traceDataDesc.numSamples));
          Log('<checksum> = ' + IntToStr(traceDataDesc.checksum));

          if cs = traceDataDesc.checksum then
          begin
            Log('Checksum OK');
            result := true;
          end
          else
            Log(format('Checksum error: %d <> %d', [traceDataDesc.checksum, cs]));
        end
      end
      else
        Log('Invalid separator found. Expected "," but found: "' + s[1 + SizeOf(TTraceAdmin) + 1]);
    end;
  end;
end;

end.

