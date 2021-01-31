program TrueBSplineInterpolation;

uses
  System.StartUpCopy,
  FMX.Forms,
  Main in 'Main.pas' {Form1},
  LUX in '_LIBRARY\LUXOPHIA\LUX\LUX.pas',
  LUX.D1 in '_LIBRARY\LUXOPHIA\LUX\LUX.D1.pas',
  LUX.D2 in '_LIBRARY\LUXOPHIA\LUX\LUX.D2.pas',
  LUX.D3 in '_LIBRARY\LUXOPHIA\LUX\LUX.D3.pas',
  LUX.D4 in '_LIBRARY\LUXOPHIA\LUX\LUX.D4.pas',
  LUX.D5 in '_LIBRARY\LUXOPHIA\LUX\LUX.D5.pas',
  LUX.DN in '_LIBRARY\LUXOPHIA\LUX\LUX.DN.pas',
  LUX.D2x2 in '_LIBRARY\LUXOPHIA\LUX\LUX.D2x2.pas',
  LUX.D2x4 in '_LIBRARY\LUXOPHIA\LUX\LUX.D2x4.pas',
  LUX.D2x4x4 in '_LIBRARY\LUXOPHIA\LUX\LUX.D2x4x4.pas',
  LUX.D3x3 in '_LIBRARY\LUXOPHIA\LUX\LUX.D3x3.pas',
  LUX.D3x4 in '_LIBRARY\LUXOPHIA\LUX\LUX.D3x4.pas',
  LUX.D3x4x4 in '_LIBRARY\LUXOPHIA\LUX\LUX.D3x4x4.pas',
  LUX.D4x4 in '_LIBRARY\LUXOPHIA\LUX\LUX.D4x4.pas',
  LUX.D4x4x4 in '_LIBRARY\LUXOPHIA\LUX\LUX.D4x4x4.pas',
  LUX.Data.Tree.core in '_LIBRARY\LUXOPHIA\LUX\Data\Tree\LUX.Data.Tree.core.pas',
  LUX.Data.Tree.Node in '_LIBRARY\LUXOPHIA\LUX\Data\Tree\LUX.Data.Tree.Node.pas',
  LUX.Data.Tree in '_LIBRARY\LUXOPHIA\LUX\Data\Tree\LUX.Data.Tree.pas',
  LUX.Draw.Shape.Chart in '_LIBRARY\LUXOPHIA\LUX.Draw\LUX.Draw.Shape.Chart.pas',
  LUX.Draw.Shape in '_LIBRARY\LUXOPHIA\LUX.Draw\LUX.Draw.Shape.pas',
  LUX.Draw.Viewer in '_LIBRARY\LUXOPHIA\LUX.Draw\LUX.Draw.Viewer.pas' {DrawViewer: TFrame},
  LUX.Draw.Scene in '_LIBRARY\LUXOPHIA\LUX.Draw\LUX.Draw.Scene.pas',
  CurveChartFrame in 'CurveChartFrame.pas' {CurveChart: TFrame},
  GenPoins in 'GenPoins.pas',
  LUX.Curve.D2 in '_LIBRARY\LUXOPHIA\LUX\Curve\LUX.Curve.D2.pas',
  LUX.Curve.BSpline in '_LIBRARY\LUXOPHIA\LUX\Curve\LUX.Curve.BSpline.pas',
  LUX.Curve in '_LIBRARY\LUXOPHIA\LUX\Curve\LUX.Curve.pas',
  LUX.Curve.Bezier in '_LIBRARY\LUXOPHIA\LUX\Curve\LUX.Curve.Bezier.pas',
  LUX.Curve.Poly in '_LIBRARY\LUXOPHIA\LUX\Curve\LUX.Curve.Poly.pas',
  LUX.Curve.CatRom in '_LIBRARY\LUXOPHIA\LUX\Curve\LUX.Curve.CatRom.pas',
  LUX.Curve.Bezier.D2 in '_LIBRARY\LUXOPHIA\LUX\Curve\LUX.Curve.Bezier.D2.pas',
  LUX.Curve.BSpline.D2 in '_LIBRARY\LUXOPHIA\LUX\Curve\LUX.Curve.BSpline.D2.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
