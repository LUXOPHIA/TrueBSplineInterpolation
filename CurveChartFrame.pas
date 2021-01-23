unit CurveChartFrame;

interface //#################################################################### ■

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  LUX, LUX.D1, LUX.D2,
  LUX.Draw.Scene, LUX.Draw.Shape, LUX.Draw.Shape.Chart, LUX.Draw.Viewer;

type //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【型】

     //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【クラス】

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TCurveChart

     TCurveChart = class( TDrawViewer )
     private
       { private 宣言 }
       _CurvMinI :Integer;
       _CurvMaxI :Integer;
       _Scene    :TDrawScene;
       _Grids    :TDrawGrids;
       _Poins    :TDrawPosCopys;
       _Verts    :TDrawPosCopys;
       _Curv     :TDrawCurv;
       ///// アクセス
       function GetCurvMinI :Integer;
       procedure SetCurvMinI( const CurvMinI_:Integer );
       function GetCurvMaxI :Integer;
       procedure SetCurvMaxI( const CurvMaxI_:Integer );
       ///// メソッド
       procedure InitChart;
     public
       { public 宣言 }
       constructor Create( Owner_:TComponent ); override;
       procedure AfterConstruction; override;
       destructor Destroy; override;
       ///// プロパティ
       property CurvMinI :Integer       read GetCurvMinI write SetCurvMinI;
       property CurvMaxI :Integer       read GetCurvMaxI write SetCurvMaxI;
       property Poins    :TDrawPosCopys read   _Poins                     ;
       property Verts    :TDrawPosCopys read   _Verts                     ;
       property Curv     :TDrawCurv     read   _Curv                      ;
     end;

implementation //############################################################### ■

{$R *.fmx}

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【クラス】

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TCurveChart

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

/////////////////////////////////////////////////////////////////////// アクセス

function TCurveChart.GetCurvMinI :Integer;
begin
     Result := _CurvMinI;
end;

procedure TCurveChart.SetCurvMinI( const CurvMinI_:Integer );
begin
     _CurvMinI := CurvMinI_;  InitChart;
end;

function TCurveChart.GetCurvMaxI :Integer;
begin
     Result := _CurvMaxI;
end;

procedure TCurveChart.SetCurvMaxI( const CurvMaxI_:Integer );
begin
     _CurvMaxI := CurvMaxI_;  InitChart;
end;

/////////////////////////////////////////////////////////////////////// メソッド

procedure TCurveChart.InitChart;
begin
     Camera.Area := TSingleArea2D.Create( _CurvMinI-2, -3, _CurvMaxI+2, +3 );

     with _Grids do
     begin
          Axis .Area := TSingleArea2D.Create( _CurvMinI-2, -3, _CurvMaxI+2, +3 );
          Grid1.Area := TSingleArea2D.Create( _CurvMinI-1, -2, _CurvMaxI+1, +2 );
          Grid2.Area := TSingleArea2D.Create( _CurvMinI  , -2, _CurvMaxI  , +2 );
          Grid3.Area := TSingleArea2D.Create( _CurvMinI  , -2, _CurvMaxI  , +2 );
     end;

     Curv .PoinsN := 8 * ( CurvMaxI - CurvMinI ) + 1;
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TCurveChart.Create( Owner_:TComponent );
begin
     inherited;

     _Scene := TDrawScene.Create;

     Camera := TDrawCamera.Create( _Scene );

     _Grids := TDrawGrids   .Create( _Scene );
     _Verts := TDrawPosCopys.Create( _Scene );
     _Curv  := TDrawCurv    .Create( _Scene );
     _Poins := TDrawPosCopys.Create( _Scene );

     with TDrawCirc.Create( _Poins ) do
     begin
          Radius           := 0.1;
          Stroke           := TStrokeBrush.Create( TBrushKind.Solid, 1 );
          Stroke.Color     := TAlphaColorF.Create( 255/255, 133/255, 133/255 ).ToAlphaColor;
          Stroke.Thickness := 0.05;
          Filler           := TBrush.Create( TBrushKind.None, 1 );
     end;

     with TDrawCirc.Create( _Verts ) do
     begin
          Radius           := 0.1;
          Stroke           := TStrokeBrush.Create( TBrushKind.Solid, 1 );
          Stroke.Color     := TAlphaColorF.Create( 1, 1, 1 ).ToAlphaColor;
          Stroke.Thickness := 0.05;
          Filler           := TBrush.Create( TBrushKind.Solid, 1 );
          Filler.Color     := TAlphaColorF.Create( 115/255, 222/255, 115/255 ).ToAlphaColor;
     end;

     with _Curv do
     begin
          Stroke           := TStrokeBrush.Create( TBrushKind.Solid, 1 );
          Stroke.Color     := TAlphaColorF.Create( 133/255, 133/255, 255/255 ).ToAlphaColor;
          Stroke.Cap       := TStrokeCap.Round;
          Stroke.Thickness := 0.1;
     end;
end;

procedure TCurveChart.AfterConstruction;
begin
     inherited;

     CurvMinI := 0;
     CurvMaxI := 8;
end;

destructor TCurveChart.Destroy;
begin
     _Scene.Free;

     inherited;
end;

end. //######################################################################### ■
