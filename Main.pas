unit Main;

interface //#################################################################### ■

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  CurveChartFrame, LUX.Draw.Viewer, FMX.Controls.Presentation, FMX.StdCtrls,
  LUX, LUX.D1, LUX.D2,
  GenPoins, Core, FMX.Edit, FMX.EditBox, FMX.SpinBox;

type
  TForm1 = class(TForm)
    CurveChart1: TCurveChart;
    Panel1: TPanel;
      LabelFN: TLabel;
        SpinBoxFN: TSpinBox;
      LabelCN: TLabel;
        SpinBoxCN: TSpinBox;
    Timer1: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure SpinBoxCNChange(Sender: TObject);
    procedure SpinBoxFNChange(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { private 宣言 }
    _FrameI :Integer;
  public
    { public 宣言 }
    _Poins  :TGenPoins;
    _Interp :TBSInterp;
    ///// メソッド
    procedure InitCurve;
    procedure MakeCurve;
    procedure ShowCurve;
  end;

var
  Form1: TForm1;

implementation //############################################################### ■

{$R *.fmx}

uses System.Math.Vectors;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

/////////////////////////////////////////////////////////////////////// メソッド

procedure TForm1.InitCurve;
begin
     with _Interp do
     begin
          _Poins.PoinMinI := PoinMinI;
          _Poins.PoinMaxI := PoinMaxI;

          CurveChart1.CurvMinI     := CurvMinI;
          CurveChart1.CurvMaxI     := CurvMaxI;
          CurveChart1.Poins.PosesN :=       PoinMaxI - PoinMinI   + 1;
          CurveChart1.Verts.PosesN :=       VertMaxI - VertMinI   + 1;
     end;
end;

procedure TForm1.MakeCurve;
const
     FN = 20{Frame};
var
   Fd, I :Integer;
begin
     Fd := _FrameI mod FN;

     if Fd = 0 then _Poins.Next;

     with _Interp do
     begin
          for I := PoinMinI to PoinMaxI do
          begin
               Poins[ I ] := _Poins.Poins( I, Fd / FN );
          end;
     end;

     Inc( _FrameI );
end;

procedure TForm1.ShowCurve;
var
   I, J :Integer;
   X :Single;
begin
     with _Interp do
     begin
          for I := 0 to CurveChart1.Poins.PosesN-1 do
          begin
               J := PoinMinI + I;

               CurveChart1.Poins.Poses[ I ] := TSingle2D.Create( J, Poins[ J ] );
          end;

          for I := 0 to CurveChart1.Verts.PosesN-1 do
          begin
               J := VertMinI + I;

               CurveChart1.Verts.Poses[ I ] := TSingle2D.Create( J, Verts[ J ] );
          end;

          for I := 0 to CurveChart1.Curv.PoinsN-1 do
          begin
               X := ( CurvMaxI - CurvMinI ) * I / ( CurveChart1.Curv.PoinsN-1 ) + CurvMinI;

               CurveChart1.Curv.Poins[ I ] := TPointF.Create( X, Curv( X ) );
          end;
     end;

     CurveChart1.Repaint;
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

procedure TForm1.FormCreate(Sender: TObject);
begin
     _Poins := TGenPoins.Create;

     _Interp := TBSInterp.Create;

     with _Interp do
     begin
          FilterW  := Round( SpinBoxFN.Value );
          CurvMinI := 0;
          CurvMaxI := Round( SpinBoxCN.Value );
     end;

     InitCurve;

     _FrameI := 0;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
     _Interp.Free;

     _Poins.Free;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TForm1.SpinBoxFNChange(Sender: TObject);
begin
     _Interp.FilterW := Round( SpinBoxFN.Value );

     InitCurve;
end;

procedure TForm1.SpinBoxCNChange(Sender: TObject);
begin
     _Interp.CurvMaxI := Round( SpinBoxCN.Value );

     InitCurve;
end;

//------------------------------------------------------------------------------

procedure TForm1.Timer1Timer(Sender: TObject);
begin
     MakeCurve;

     ShowCurve;
end;

end. //######################################################################### ■
