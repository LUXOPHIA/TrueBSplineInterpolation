unit Main;

interface //#################################################################### ■

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.StdCtrls, FMX.Edit, FMX.EditBox, FMX.SpinBox, FMX.Controls.Presentation,
  LUX, LUX.D1, LUX.D2,
  LUX.Curve.BSpline,
  LUX.Draw.Viewer,
  GenPoins, CurveChartFrame;

type
  TForm1 = class(TForm)
    CurveChart1: TCurveChart;
    Panel1: TPanel;
      Panel2: TPanel;
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
    _Interp :TSingleBSInterp;
    ///// メソッド
    procedure InitChart;
    procedure MakePoins( const Td_:Single );
    procedure MakeCurve;
  end;

var
  Form1: TForm1;

implementation //############################################################### ■

{$R *.fmx}

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

/////////////////////////////////////////////////////////////////////// メソッド

procedure TForm1.InitChart;
begin
     with _Interp do
     begin
          _Poins.MinI := PoinMinI;
          _Poins.MaxI := PoinMaxI;

          CurveChart1.PoinMinI := PoinMinI;
          CurveChart1.PoinMaxI := PoinMaxI;
          CurveChart1.VertMinI := VertMinI;
          CurveChart1.VertMaxI := VertMaxI;
          CurveChart1.CurvMinI := CurvMinI;
          CurveChart1.CurvMaxI := CurvMaxI;
     end;
end;

procedure TForm1.MakePoins( const Td_:Single );
var
   I :Integer;
begin
     with _Interp do
     begin
          for I := PoinMinI to PoinMaxI do
          begin
               Poins[ I ] := _Poins.Poins( I, Td_ );

               CurveChart1.Poins.PosYs[ I ] := Poins[ I ];
          end;
     end;
end;

procedure TForm1.MakeCurve;
var
   I :Integer;
   X :Single;
begin
     with _Interp do
     begin
          for I := VertMinI to VertMaxI do
          begin
               CurveChart1.Verts.PosYs[ I ] := Verts[ I ];
               CurveChart1.Lines.PosYs[ I ] := Verts[ I ];
          end;
     end;

     with CurveChart1.Curv do
     begin
          for I := 0 to DivN do
          begin
               X := ( MaxX - MinX ) * I / DivN + MinX;

               Poins[ I ] := TSingle2D.Create( X, _Interp.Curv( X ) );
          end;
     end;
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

procedure TForm1.FormCreate(Sender: TObject);
begin
     _Poins := TGenPoins.Create;

     _Interp := TSingleBSInterp.Create;

     with _Interp do
     begin
          FilterW  := Round( SpinBoxFN.Value );
          CurvMinI := 0;
          CurvMaxI := Round( SpinBoxCN.Value );
     end;

     InitChart;

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

     InitChart;
end;

procedure TForm1.SpinBoxCNChange(Sender: TObject);
begin
     _Interp.CurvMaxI := Round( SpinBoxCN.Value );

     InitChart;
end;

//------------------------------------------------------------------------------

procedure TForm1.Timer1Timer(Sender: TObject);
const
     FN = 20{Frame};
var
   Fd :Integer;
begin
     Fd := _FrameI mod FN;

     if Fd = 0 then _Poins.Next;

     MakePoins( Fd / FN );

     MakeCurve;

     Inc( _FrameI );

     CurveChart1.Repaint;
end;

end. //######################################################################### ■
