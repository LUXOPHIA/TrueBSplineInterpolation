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
    procedure MakePoins( const Td_:Single );
    procedure MakeCurve;
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

          CurveChart1.CurvMinI    := CurvMinI;
          CurveChart1.CurvMaxI    := CurvMaxI;
          CurveChart1.Curv.PoinsN := 8 * ( CurvMaxI - CurvMinI ) + 1;
          CurveChart1.Poins.MinI  := PoinMinI;
          CurveChart1.Poins.MaxI  := PoinMaxI;
          CurveChart1.Verts.MinI  := VertMinI;
          CurveChart1.Verts.MaxI  := VertMaxI;
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
          for I := VertMinI to VertMaxI do CurveChart1.Verts.PosYs[ I ] := Verts[ I ];

          for I := 0 to CurveChart1.Curv.PoinsN-1 do
          begin
               X := ( CurvMaxI - CurvMinI ) * I / ( CurveChart1.Curv.PoinsN-1 ) + CurvMinI;

               CurveChart1.Curv.Poins[ I ] := TSingle2D.Create( X, Curv( X ) );
          end;
     end;
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
