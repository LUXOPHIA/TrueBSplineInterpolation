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
       _AxisY  :TDrawAxisY;
       _Grid1  :TDrawGrid;
       _Grid2  :TDrawGrid;
       _Grid3  :TDrawGrid;
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
       property CellsN :Integer read GetCellsN write SetCellsN;
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

     _Grid3.Area := TSingleArea2D.Create(  0, -2, _CellsN  , +2 );
     _Grid2.Area := TSingleArea2D.Create(  0, -2, _CellsN  , +2 );
     _Grid1.Area := TSingleArea2D.Create( -1, -2, _CellsN+1, +2 );
     _AxisY.Area := TSingleArea2D.Create( -2, -2, _CellsN+2, +2 );

     Camera.Area := TSingleArea2D.Create( -2, -3, _CellsN+2, +3 );
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TCurveChart.Create( Owner_:TComponent );
begin
     inherited;

     _Scene := TDrawScene.Create;

     Camera := TDrawCamera.Create( _Scene );

     _Grid3 := TDrawGrid .Create( _Scene );
     _Grid2 := TDrawGrid .Create( _Scene );
     _Grid1 := TDrawGrid .Create( _Scene );
     _AxisY := TDrawAxisY.Create( _Scene );

     with _Grid3 do
     begin
          Interv := 1/10;
          Stroke := TStrokeBrush.Create( TBrushKind.Solid, TAlphaColorF.Create( 15/16, 15/16, 15/16 ).ToAlphaColor );
          Stroke.Thickness := 0.02;
     end;

     with _Grid2 do
     begin
          Interv := 1/2;
          Stroke := TStrokeBrush.Create( TBrushKind.Solid, TAlphaColorF.Create( 7/8, 7/8, 7/8 ).ToAlphaColor );
          Stroke.Thickness := 0.02;
     end;

     with _Grid1 do
     begin
          Interv := 1;
          Stroke := TStrokeBrush.Create( TBrushKind.Solid, TAlphaColorF.Create( 3/4, 3/4, 3/4 ).ToAlphaColor );
          Stroke.Thickness := 0.02;
     end;

     with _AxisY do
     begin
          Stroke := TStrokeBrush.Create( TBrushKind.Solid, TAlphaColorF.Create( 1/2, 1/2, 1/2 ).ToAlphaColor );
          Stroke.Thickness := 0.02;
     end;

     _Conts := TDrawCircs.Create( _Scene );
     _Curv  := TDrawCurv .Create( _Scene );
     _Poins := TDrawCircs.Create( _Scene );

     CellsN := 8
end;

destructor TCurveChart.Destroy;
begin
     _Scene.Free;

     inherited;
end;

end. //######################################################################### ■
