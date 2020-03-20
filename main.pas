unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Menus,
  ExtCtrls, ComCtrls, TAGraph, TASeries, TATransformations, fluke12x, types,
  serialconnect;

type

  { TForm1 }

  TForm1 = class(TForm)
    ASeries: TLineSeries;
    BSeries: TLineSeries;
    Chart1: TChart;
    Memo1: TMemo;
    PageControl1: TPageControl;
    RightAxisTransform: TChartAxisTransformations;
    LinearAxisTransform: TLinearAxisTransform;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    StatusBar1: TStatusBar;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure MenuItem1Click(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
    procedure MenuItem3Click(Sender: TObject);
    procedure MenuItem4Click(Sender: TObject);
  private
    FFluke: TFluke12X;
    procedure Log(const s: string);
    procedure plotTrace(traceAdmin: TTraceAdmin; traceDataDesc: TTraceData;
      data: TIntegerDynArray; seriesIndex: integer);
    function traceGrabA: boolean;
    function traceGrabB: boolean;
  public

  end;

var
  Form1: TForm1;

implementation

uses
  Math;

var
  portName: string = '/dev/ttyUSB0';

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if Assigned(FFluke) then
  begin
    FFluke.executeCmd(cmdBaud1200, 500);
    FreeAndNil(FFluke);
  end;
end;

procedure TForm1.MenuItem1Click(Sender: TObject);
var
  SerialForm: TForm2;
  s: string;
  mr: TModalResult;
begin
  SerialForm := TForm2.Create(nil);
  SerialForm.ShowModal;
  mr := SerialForm.ModalResult;
  s := SerialForm.ComboBox1.Text;
  SerialForm.Free;
  if mr <> mrOK then
    exit
  else
    portName := s;

  try
    Screen.Cursor := crHourGlass;
    StatusBar1.Panels[3].Text := 'Connecting...';
    Application.ProcessMessages;

    if not Assigned(FFluke) then
    begin
      FFluke := TFluke12X.Create;
      FFluke.DoLog := @Log;
      if FFluke.connectFluke(portName, s) then
      begin
        self.Caption := copy(s, 1, pos(';', s)-1);
        MenuItem2.Enabled := true;
        MenuItem3.Enabled := true;
        MenuItem4.Enabled := true;
        StatusBar1.Panels[1].Text := portName;
        StatusBar1.Panels[3].Text := 'OK';
        MenuItem1.Enabled := false;
      end
      else
      begin
        StatusBar1.Panels[1].Text := '-';
        StatusBar1.Panels[3].Text := 'Couldn''t connect';
        FreeAndNil(FFluke);
      end;
    end;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TForm1.MenuItem2Click(Sender: TObject);
var
  SL: TStringList;
begin
  try
    Screen.Cursor := crHourGlass;
    StatusBar1.Panels[3].Text := 'Collecting screengrab...';
    Application.ProcessMessages;
    SL := TStringList.Create;
    FFluke.screenGrabAsPS(TStrings(SL));
    SL.SaveToFile('screengrab.ps');
  finally
    SL.Free;
    Screen.Cursor := crDefault;
    StatusBar1.Panels[3].Text := 'OK';
  end;
end;

procedure TForm1.MenuItem3Click(Sender: TObject);
begin
  try
    Screen.Cursor := crHourGlass;
    StatusBar1.Panels[3].Text := 'Collecting trace A data';
    Application.ProcessMessages;
    traceGrabA;
  finally
    Screen.Cursor := crDefault;
    StatusBar1.Panels[3].Text := 'OK';
  end;
end;

procedure TForm1.MenuItem4Click(Sender: TObject);
begin
  try
    Screen.Cursor := crHourGlass;
    StatusBar1.Panels[3].Text := 'Collecting trace B data';
    Application.ProcessMessages;
    traceGrabB;
  finally
    Screen.Cursor := crDefault;
    StatusBar1.Panels[3].Text := 'OK';
  end;
end;

procedure TForm1.Log(const s: string);
begin
  Memo1.Lines.Add(s);
end;

// Order of magnitute round
// 1234 => 1300
// 9.02 => 9.1
// -12  => -13
function OOMRoundUp(val: double): double;
const
  sigfig = 4;
var
  mag: integer;
  normbound: double;
begin
  mag := trunc(log10(abs(val)));
  normbound := abs(val) / intpower(10, mag-(sigfig - 1));
  result:= sign(val)*round(normbound+0.4999)*intpower(10, mag-(sigfig - 1));
end;

procedure TForm1.plotTrace(traceAdmin: TTraceAdmin; traceDataDesc: TTraceData;
  data: TIntegerDynArray; seriesIndex: integer);
var
  i: integer;
  x, y, min, max, scale, offset: double;
begin
  Chart1.AxisList.BottomAxis.Title.Caption := flukeUnits[traceadmin.x_unit];
  Chart1.AxisList.BottomAxis.Title.Visible := true;

  // Rely on y-axis indexing the same as series index
  min := OOMRoundUp(traceDataDesc.underload_value*traceadmin.y_res + traceadmin.y_zero);
  max := OOMRoundUp(traceDataDesc.overload_value*traceadmin.y_res + traceadmin.y_zero);

  Chart1.AxisList[seriesIndex].Range.Min := min;
  Chart1.AxisList[seriesIndex].Range.Max := max;
  Chart1.AxisList[seriesIndex].Range.UseMax := true;
  Chart1.AxisList[seriesIndex].Range.UseMin := true;
  Chart1.AxisList[seriesIndex].Marks.Range.Min := min;
  Chart1.AxisList[seriesIndex].Marks.Range.Max := max;
  Chart1.AxisList[seriesIndex].Marks.Range.UseMax := true;
  Chart1.AxisList[seriesIndex].Marks.Range.UseMin := true;
  Chart1.AxisList[seriesIndex].Title.Caption := flukeUnits[traceadmin.y_unit];
  Chart1.AxisList[seriesIndex].Title.Visible := true;
  Chart1.AxisList[seriesIndex].Visible := true;

  // Scale right axis to match left axis grid
  if (seriesIndex > 0) and (max > min) then
  begin
    scale := (Chart1.LeftAxis.Range.Max - Chart1.LeftAxis.Range.Min) / (max-min);
    offset := Chart1.LeftAxis.Range.Min - scale*min;
    LinearAxisTransform.Scale := scale;
    LinearAxisTransform.Offset := offset;
  end;

  TLineSeries(Chart1.Series.Items[seriesIndex]).Clear;

  for i := 0 to traceDataDesc.numSamples-1 do
  begin
    x := i;
    if traceDataDesc.MinMaxPairs then
    begin
      y := (data[i] + data[i+1]);
      TLineSeries(Chart1.Series.Items[seriesIndex]).AddXY(x*traceadmin.x_res,
                  y*traceadmin.y_res + traceadmin.y_zero);
    end
    else
    begin
      y := data[i];
      TLineSeries(Chart1.Series.Items[seriesIndex]).AddXY(x*traceadmin.x_res,
        y*traceadmin.y_res + traceadmin.y_zero);
    end;
  end;
end;

function TForm1.traceGrabA: boolean;
var
  TA: TTraceAdmin;
  TD: TTraceData;
  data: TIntegerDynArray;
begin
  result := FFluke.traceGrab(cmdTraceGrabA_setup_data, TA, TD, data);
  if result then
    plotTrace(TA, TD, data, 0);
end;

function TForm1.traceGrabB: boolean;
var
  TA: TTraceAdmin;
  TD: TTraceData;
  data: TIntegerDynArray;
begin
  result := FFluke.traceGrab(cmdTraceGrabB_setup_data, TA, TD, data);
  if result then
    plotTrace(TA, TD, data, 1);
end;

end.

