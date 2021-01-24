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
       _PoinMinI :Integer;
       _PoinMaxI :Integer;
       _VertMinI :Integer;
       _VertMaxI :Integer;
       _CurvMinI :Integer;
       _CurvMaxI :Integer;
       _Scene    :TDrawScene;
       _Grids    :TDrawGrids;
       _Lines    :TDrawLines1D;
       _Verts    :TDrawCopys1D;
       _Curv     :TDrawCurv1D;
       _Poins    :TDrawPoins1D;
       ///// アクセス
       function GetPoinMinI :Integer;
       procedure SetPoinMinI( const PoinMinI_:Integer );
       function GetPoinMaxI :Integer;
       procedure SetPoinMaxI( const PoinMaxI_:Integer );
       function GetVertMinI :Integer;
       procedure SetVertMinI( const VertMinI_:Integer );
       function GetVertMaxI :Integer;
       procedure SetVertMaxI( const VertMaxI_:Integer );
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
       property PoinMinI :Integer      read GetPoinMinI write SetPoinMinI;
       property PoinMaxI :Integer      read GetPoinMaxI write SetPoinMaxI;
       property VertMinI :Integer      read GetVertMinI write SetVertMinI;
       property VertMaxI :Integer      read GetVertMaxI write SetVertMaxI;
       property CurvMinI :Integer      read GetCurvMinI write SetCurvMinI;
       property CurvMaxI :Integer      read GetCurvMaxI write SetCurvMaxI;
       property Lines    :TDrawLines1D read   _Lines                     ;
       property Verts    :TDrawCopys1D read   _Verts                     ;
       property Curv     :TDrawCurv1D  read   _Curv                      ;
       property Poins    :TDrawPoins1D read   _Poins                     ;
     end;

implementation //############################################################### ■

{$R *.fmx}

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【クラス】

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TCurveChart

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

/////////////////////////////////////////////////////////////////////// アクセス

function TCurveChart.GetPoinMinI :Integer;
begin
     Result := _PoinMinI;
end;

procedure TCurveChart.SetPoinMinI( const PoinMinI_:Integer );
begin
     _PoinMinI := PoinMinI_;  InitChart;
end;

function TCurveChart.GetPoinMaxI :Integer;
begin
     Result := _PoinMaxI;
end;

procedure TCurveChart.SetPoinMaxI( const PoinMaxI_:Integer );
begin
     _PoinMaxI := PoinMaxI_;  InitChart;
end;

function TCurveChart.GetVertMinI :Integer;
begin
     Result := _VertMinI;
end;

procedure TCurveChart.SetVertMinI( const VertMinI_:Integer );
begin
     _VertMinI := VertMinI_;  InitChart;
end;

function TCurveChart.GetVertMaxI :Integer;
begin
     Result := _VertMaxI;
end;

procedure TCurveChart.SetVertMaxI( const VertMaxI_:Integer );
begin
     _VertMaxI := VertMaxI_;  InitChart;
end;

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
     Camera.Area := TSingleArea2D.Create( VertMinI-1, -3, VertMaxI+1, +3 );

     with _Grids do
     begin
          Axis .Area := TSingleArea2D.Create( VertMinI-1, -3, VertMaxI+1, +3 );
          Grid1.Area := TSingleArea2D.Create( VertMinI  , -2, VertMaxI  , +2 );
          Grid2.Area := TSingleArea2D.Create( CurvMinI  , -2, CurvMaxI  , +2 );
          Grid3.Area := TSingleArea2D.Create( CurvMinI  , -2, CurvMaxI  , +2 );
     end;

     Lines.MinX := VertMinI;
     Lines.MaxX := VertMaxI;

     Verts.MinI := VertMinI;
     Verts.MaxI := VertMaxI;

     Curv .MinX := CurvMinI;
     Curv .MaxX := CurvMaxI;
     Curv .DivN := 8 * ( CurvMaxI - CurvMinI );

     Poins.MinI := PoinMinI;
     Poins.MaxI := PoinMaxI;
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TCurveChart.Create( Owner_:TComponent );
begin
     inherited;

     _Scene := TDrawScene.Create;

     Camera := TDrawCamera.Create( _Scene );

     _Grids := TDrawGrids  .Create( _Scene );
     _Lines := TDrawLines1D.Create( _Scene );
     _Verts := TDrawCopys1D.Create( _Scene );
     _Curv  := TDrawCurv1D .Create( _Scene );
     _Poins := TDrawPoins1D.Create( _Scene );

     with _Lines do
     begin
          Stroke           := TStrokeBrush.Create( TBrushKind.Solid, 1 );
          Stroke.Color     := TAlphaColorF.Create( 115/255, 222/255, 115/255 ).ToAlphaColor;
          Stroke.Cap       := TStrokeCap.Round;
          Stroke.Thickness := 0.02;
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

     with _Poins.Poin do
     begin
          Radius           := 0.1;
          Stroke           := TStrokeBrush.Create( TBrushKind.Solid, 1 );
          Stroke.Color     := TAlphaColorF.Create( 255/255, 133/255, 133/255 ).ToAlphaColor;
          Stroke.Thickness := 0.05;
          Filler           := TBrush.Create( TBrushKind.None, 1 );
     end;
end;

procedure TCurveChart.AfterConstruction;
begin
     inherited;

     CurvMinI := 0;
     CurvMaxI := 8;
     VertMinI := CurvMinI - 1;
     VertMaxI := CurvMaxI + 1;
     PoinMinI := VertMinI - 4;
     PoinMaxI := VertMaxI + 4;
end;

destructor TCurveChart.Destroy;
begin
     _Scene.Free;

     inherited;
end;

end. //######################################################################### ■
