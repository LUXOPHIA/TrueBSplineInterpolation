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

     CellsN := 8
end;

destructor TCurveChart.Destroy;
begin
     _Scene.Free;

     inherited;
end;

end. //######################################################################### ■
