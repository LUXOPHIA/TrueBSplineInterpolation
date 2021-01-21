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
       _CellsN :Integer;
       _Scene  :TDrawScene;
       _Grids  :TDrawGrids;
       _Poins  :TDrawCircs;
       _Conts  :TDrawCircs;
       _Curv   :TDrawCurv;
       ///// アクセス
       function GetCellsN :Integer;
       procedure SetCellsN( const CellsN_:Integer );
     public
       { public 宣言 }
       constructor Create( Owner_:TComponent ); override;
       destructor Destroy; override;
       ///// プロパティ
       property CellsN :Integer    read GetCellsN write SetCellsN;
       property Poins  :TDrawCircs read   _Poins                 ;
       property Conts  :TDrawCircs read   _Conts                 ;
       property Curv   :TDrawCurv  read   _Curv                  ;
     end;

implementation //############################################################### ■

{$R *.fmx}

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【クラス】

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TCurveChart

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

function TCurveChart.GetCellsN :Integer;
begin
     Result := _CellsN;
end;

procedure TCurveChart.SetCellsN( const CellsN_:Integer );
begin
     _CellsN := CellsN_;

     with _Grids do
     begin
          Axis .Area := TSingleArea2D.Create( -2, -3, _CellsN+2, +3 );
          Grid1.Area := TSingleArea2D.Create( -1, -2, _CellsN+1, +2 );
          Grid2.Area := TSingleArea2D.Create(  0, -2, _CellsN  , +2 );
          Grid3.Area := TSingleArea2D.Create(  0, -2, _CellsN  , +2 );
     end;

     Camera.Area := TSingleArea2D.Create( -2, -3, _CellsN+2, +3 );
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TCurveChart.Create( Owner_:TComponent );
begin
     inherited;

     _Scene := TDrawScene.Create;

     Camera := TDrawCamera.Create( _Scene );

     _Grids := TDrawGrids.Create( _Scene );
     _Conts := TDrawCircs.Create( _Scene );
     _Curv  := TDrawCurv .Create( _Scene );
     _Poins := TDrawCircs.Create( _Scene );

     _Conts.Filler := TBrush      .Create( TBrushKind.Solid, 1 );
     _Curv .Stroke := TStrokeBrush.Create( TBrushKind.Solid, 1 );
     _Poins.Stroke := TStrokeBrush.Create( TBrushKind.Solid, 1 );

     with _Conts do
     begin
          Radius           := 0.2;
          Filler.Color     := TAlphaColorF.Create( 0.5, 1.0, 0.5 ).ToAlphaColor;
     end;
     with _Curv do
     begin
          Stroke.Color     := TAlphaColorF.Create( 1.0, 0.5, 0.5 ).ToAlphaColor;
          Stroke.Thickness := 0.01;
     end;
     with _Poins do
     begin
          Radius           := 0.2;
          Stroke.Color     := TAlphaColorF.Create( 0.5, 0.5, 1.0 ).ToAlphaColor;
          Stroke.Thickness := 0.01;
     end;

     CellsN := 8
end;

destructor TCurveChart.Destroy;
begin
     _Scene.Free;

     inherited;
end;

end. //######################################################################### ■
