unit LUX.Draw.Shape.Chart;

interface //#################################################################### ■

uses System.Types, System.UITypes,
     FMX.Graphics,
     LUX, LUX.D1, LUX.D2,
     LUX.Draw.Scene,
     LUX.Draw.Shape,
     LUX.Draw.Viewer;

type //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【型】

     TDrawScal    = class;
       TDrawScalX = class;
       TDrawScalY = class;
       TDrawGrid  = class;
     TDrawAxisX   = class;
     TDrawAxisY   = class;
     TDrawAxis    = class;
     TDrawGrids   = class;

     //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【クラス】

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TDrawScal

     TDrawScal = class( TDrawShape )
     private
     protected
       _Interv :Single;
       ///// アクセス
       function GetInterv :Single; virtual;
       procedure SetInterv( const Interv_:Single ); virtual;
     public
       constructor Create; override;
       procedure AfterConstruction; override;
       destructor Destroy; override;
       ///// プロパティ
       property Interv :Single read GetInterv write SetInterv;
       ///// メソッド
     end;

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TDrawScalX

     TDrawScalX = class( TDrawScal )
     private
     protected
       ///// メソッド
       procedure DrawMain( const Canvas_:TCanvas ); override;
     public
       constructor Create; override;
       procedure AfterConstruction; override;
       destructor Destroy; override;
       ///// プロパティ
       ///// メソッド
     end;

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TDrawScalY

     TDrawScalY = class( TDrawScal )
     private
     protected
       ///// メソッド
       procedure DrawMain( const Canvas_:TCanvas ); override;
     public
       constructor Create; override;
       procedure AfterConstruction; override;
       destructor Destroy; override;
       ///// プロパティ
       ///// メソッド
     end;

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TDrawGrid

     TDrawGrid = class( TDrawScal )
     private
     protected
       _ScalX :TDrawScalX;
       _ScalY :TDrawScalY;
       ///// アクセス
       procedure SetRelaArea( const Area_:TSingleArea2D ); override;
       function GetInterv :Single; override;
       procedure SetInterv( const Interv_:Single ); override;
     public
       constructor Create; override;
       procedure AfterConstruction; override;
       destructor Destroy; override;
       ///// プロパティ
       property ScalX :TDrawScalX read _ScalX;
       property ScalY :TDrawScalY read _ScalY;
       ///// メソッド
     end;

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TDrawAxisX

     TDrawAxisX = class( TDrawShape )
     private
     protected
       ///// メソッド
       procedure DrawMain( const Canvas_:TCanvas ); override;
     public
       constructor Create; override;
       procedure AfterConstruction; override;
       destructor Destroy; override;
       ///// プロパティ
       ///// メソッド
     end;

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TDrawAxisY

     TDrawAxisY = class( TDrawShape )
     private
     protected
       ///// メソッド
       procedure DrawMain( const Canvas_:TCanvas ); override;
     public
       constructor Create; override;
       procedure AfterConstruction; override;
       destructor Destroy; override;
       ///// プロパティ
       ///// メソッド
     end;

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TDrawAxis

     TDrawAxis = class( TDrawShape )
     private
     protected
       _AxisX :TDrawAxisX;
       _AxisY :TDrawAxisY;
       ///// アクセス
       procedure SetRelaArea( const Area_:TSingleArea2D ); override;
     public
       constructor Create; override;
       procedure AfterConstruction; override;
       destructor Destroy; override;
       ///// プロパティ
       property AxisX :TDrawAxisX read _AxisX;
       property AxisY :TDrawAxisY read _AxisY;
       ///// メソッド
     end;

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TDrawGrids

     TDrawGrids = class( TDrawShape )
     private
     protected
       _Axis  :TDrawAxis;
       _Grid1 :TDrawGrid;
       _Grid2 :TDrawGrid;
       _Grid3 :TDrawGrid;
       ///// アクセス
       procedure SetRelaArea( const Area_:TSingleArea2D ); override;
     public
       constructor Create; override;
       procedure AfterConstruction; override;
       destructor Destroy; override;
       ///// プロパティ
       property Axis  :TDrawAxis read _Axis ;
       property Grid1 :TDrawGrid read _Grid1;
       property Grid2 :TDrawGrid read _Grid2;
       property Grid3 :TDrawGrid read _Grid3;
       ///// メソッド
     end;

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TDrawCopys1D

     TDrawCopys1D = class( TDrawPosCopys )
     private
     protected
       _MinI :Integer;
       _MaxI :Integer;
       ///// アクセス
       function GetMinI :Integer; virtual;
       procedure SetMinI( const MinI_:Integer ); virtual;
       function GetMaxI :Integer; virtual;
       procedure SetMaxI( const MaxI_:Integer ); virtual;
       function GetPosYs( const I_:Integer ) :Single; virtual;
       procedure SetPosYs( const I_:Integer; const PosYs_:Single ); virtual;
     public
       constructor Create; override;
       procedure AfterConstruction; override;
       destructor Destroy; override;
       ///// プロパティ
       property MinI                      :Integer read GetMinI  write SetMinI ;
       property MaxI                      :Integer read GetMaxI  write SetMaxI ;
       property PosYs[ const I_:Integer ] :Single  read GetPosYs write SetPosYs;
       ///// メソッド
     end;

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TDrawPoins1D

     TDrawPoins1D = class( TDrawCopys1D )
     private
     protected
       _Poin :TDrawCirc;
       ///// アクセス
     public
       constructor Create; override;
       procedure AfterConstruction; override;
       destructor Destroy; override;
       ///// プロパティ
       property Poin :TDrawCirc read _Poin;
       ///// メソッド
     end;

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TDrawLines1D

     TDrawLines1D = class( TDrawCurv )
     private
     protected
       _MinX :Integer;
       _MaxX :Integer;
       ///// アクセス
       function GetPosYs( const I_:Integer ) :Single;
       procedure SetPosYs( const I_:Integer; const PosYs_:Single );
       function GetMinI :Integer;
       procedure SetMinI( const MinI_:Integer );
       function GetMaxI :Integer;
       procedure SetMaxI( const MaxI_:Integer );
       ///// メソッド
     public
       ///// プロパティ
       property PosYs[ const I_:Integer ] :Single  read GetPosYs write SetPosYs; default;
       property MinI                      :Integer read GetMinI  write SetMinI ;
       property MaxI                      :Integer read GetMaxI  write SetMaxI ;
       ///// メソッド
       procedure Func( const Func_:TConstFunc<Integer,Single> );
     end;

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TDrawChain1D

     TDrawChain1D = class( TDrawShape )
     private
     protected
       _Poins :TDrawPoins1D;
       _Lines :TDrawLines1D;
       ///// アクセス
       function GetPosYs( const I_:Integer ) :Single;
       procedure SetPosYs( const I_:Integer; const PosYs_:Single );
       function GetMinI :Integer;
       procedure SetMinI( const MinI_:Integer );
       function GetMaxI :Integer;
       procedure SetMaxI( const MaxI_:Integer );
     public
       constructor Create; override;
       procedure AfterConstruction; override;
       destructor Destroy; override;
       ///// プロパティ
       property PosYs[ const I_:Integer ] :Single       read GetPosYs write SetPosYs; default;
       property MinI                      :Integer      read GetMinI  write SetMinI ;
       property MaxI                      :Integer      read GetMaxI  write SetMaxI ;
       property Poins                     :TDrawPoins1D read   _Poins               ;
       property Lines                     :TDrawLines1D read   _Lines               ;
       ///// メソッド
     end;

implementation //############################################################### ■

uses System.Math;

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【クラス】

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TDrawScal

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

/////////////////////////////////////////////////////////////////////// アクセス

function TDrawScal.GetInterv :Single;
begin
     Result := _Interv;
end;

procedure TDrawScal.SetInterv( const Interv_:Single );
begin
     _Interv := Interv_;
end;

/////////////////////////////////////////////////////////////////////// メソッド

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TDrawScal.Create;
begin
     inherited;

end;

procedure TDrawScal.AfterConstruction;
begin
     inherited;

     Area := TSingleArea2D.Create( -10, -10, +10, +10 );

     Interv := 0.1;
end;

destructor TDrawScal.Destroy;
begin

     inherited;
end;

/////////////////////////////////////////////////////////////////////// メソッド

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TDrawScalX

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

/////////////////////////////////////////////////////////////////////// アクセス

/////////////////////////////////////////////////////////////////////// メソッド

procedure TDrawScalX.DrawMain( const Canvas_:TCanvas );
var
   I0, I1, I :Integer;
   X :Single;
   P0, P1 :TSingle2D;
begin
     inherited;

     I0 := Ceil ( Area.Min.X / _Interv );
     I1 := Floor( Area.Max.X / _Interv );

     P0.Y := Area.Min.Y;
     P1.Y := Area.Max.Y;
     for I := I0 to I1 do
     begin
          X := I * _Interv;

          P0.X := X;
          P1.X := X;

          Canvas_.DrawLine( P0, P1, _Opacity );
     end;
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TDrawScalX.Create;
begin
     inherited;

end;

procedure TDrawScalX.AfterConstruction;
begin
     inherited;

end;

destructor TDrawScalX.Destroy;
begin

     inherited;
end;

/////////////////////////////////////////////////////////////////////// メソッド

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TDrawScalY

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

/////////////////////////////////////////////////////////////////////// アクセス

/////////////////////////////////////////////////////////////////////// メソッド

procedure TDrawScalY.DrawMain( const Canvas_:TCanvas );
var
   I0, I1, I :Integer;
   Y :Single;
   P0, P1 :TSingle2D;
begin
     inherited;

     I0 := Ceil ( Area.Min.Y / _Interv );
     I1 := Floor( Area.Max.Y / _Interv );

     P0.X := Area.Min.X;
     P1.X := Area.Max.X;
     for I := I0 to I1 do
     begin
          Y := I * _Interv;

          P0.Y := Y;
          P1.Y := Y;

          Canvas_.DrawLine( P0, P1, _Opacity );
     end;
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TDrawScalY.Create;
begin
     inherited;

end;

procedure TDrawScalY.AfterConstruction;
begin
     inherited;

end;

destructor TDrawScalY.Destroy;
begin

     inherited;
end;

/////////////////////////////////////////////////////////////////////// メソッド

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TDrawGrid

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

/////////////////////////////////////////////////////////////////////// アクセス

function TDrawGrid.GetInterv :Single;
begin
     Result := Min( _ScalX.Interv, _ScalY.Interv );
end;

procedure TDrawGrid.SetInterv( const Interv_:Single );
begin
     _ScalX.Interv := Interv_;
     _ScalY.Interv := Interv_;
end;

/////////////////////////////////////////////////////////////////////// メソッド

procedure TDrawGrid.SetRelaArea( const Area_:TSingleArea2D );
begin
     inherited;

     _ScalX.Area := Area;
     _ScalY.Area := Area;
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TDrawGrid.Create;
begin
     inherited;

     _ScalX := TDrawScalX.Create( Self );
     _ScalY := TDrawScalY.Create( Self );
end;

procedure TDrawGrid.AfterConstruction;
begin
     inherited;

end;

destructor TDrawGrid.Destroy;
begin

     inherited;
end;

/////////////////////////////////////////////////////////////////////// メソッド

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TDrawAxisX

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

/////////////////////////////////////////////////////////////////////// アクセス

/////////////////////////////////////////////////////////////////////// メソッド

procedure TDrawAxisX.DrawMain( const Canvas_:TCanvas );
var
   P0, P1 :TSingle2D;
begin
     inherited;

     P0.X := 0;  P0.Y := Area.Min.Y;
     P1.X := 0;  P1.Y := Area.Max.Y;

     Canvas_.DrawLine( P0, P1, _Opacity );
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TDrawAxisX.Create;
begin
     inherited;

end;

procedure TDrawAxisX.AfterConstruction;
begin
     inherited;

end;

destructor TDrawAxisX.Destroy;
begin

     inherited;
end;

/////////////////////////////////////////////////////////////////////// メソッド

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TDrawAxisY

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

/////////////////////////////////////////////////////////////////////// アクセス

/////////////////////////////////////////////////////////////////////// メソッド

procedure TDrawAxisY.DrawMain( const Canvas_:TCanvas );
var
   P0, P1 :TSingle2D;
begin
     inherited;

     P0.X := Area.Min.X;  P0.Y := 0;
     P1.X := Area.Max.X;  P1.Y := 0;

     Canvas_.DrawLine( P0, P1, _Opacity );
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TDrawAxisY.Create;
begin
     inherited;

end;

procedure TDrawAxisY.AfterConstruction;
begin
     inherited;

end;

destructor TDrawAxisY.Destroy;
begin

     inherited;
end;

/////////////////////////////////////////////////////////////////////// メソッド

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TDrawAxis

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

/////////////////////////////////////////////////////////////////////// アクセス

/////////////////////////////////////////////////////////////////////// メソッド

procedure TDrawAxis.SetRelaArea( const Area_:TSingleArea2D );
begin
     inherited;

     _AxisX.Area := Area;
     _AxisY.Area := Area;
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TDrawAxis.Create;
begin
     inherited;

     _AxisX := TDrawAxisX.Create( Self );
     _AxisY := TDrawAxisY.Create( Self );
end;

procedure TDrawAxis.AfterConstruction;
begin
     inherited;

end;

destructor TDrawAxis.Destroy;
begin

     inherited;
end;

/////////////////////////////////////////////////////////////////////// メソッド

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TDrawGrids

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

/////////////////////////////////////////////////////////////////////// アクセス

/////////////////////////////////////////////////////////////////////// メソッド

procedure TDrawGrids.SetRelaArea( const Area_:TSingleArea2D );
begin
     inherited;

     _Grid3.Area := Area;
     _Grid2.Area := Area;
     _Grid1.Area := Area;
     _Axis .Area := Area;
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TDrawGrids.Create;
begin
     inherited;

     _Grid3 := TDrawGrid.Create( Self );
     _Grid2 := TDrawGrid.Create( Self );
     _Grid1 := TDrawGrid.Create( Self );
     _Axis  := TDrawAxis.Create( Self );
end;

procedure TDrawGrids.AfterConstruction;
begin
     inherited;

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

     with _Axis do
     begin
          Stroke := TStrokeBrush.Create( TBrushKind.Solid, TAlphaColorF.Create( 1/2, 1/2, 1/2 ).ToAlphaColor );
          Stroke.Thickness := 0.02;
     end;
end;

destructor TDrawGrids.Destroy;
begin

     inherited;
end;

/////////////////////////////////////////////////////////////////////// メソッド

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TDrawCopys1D

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

/////////////////////////////////////////////////////////////////////// アクセス

function TDrawCopys1D.GetMinI :Integer;
begin
     Result := _MinI;
end;

procedure TDrawCopys1D.SetMinI( const MinI_:Integer );
begin
     _MinI := MinI_;  PosesN := MaxI - MinI + 1;
end;

function TDrawCopys1D.GetMaxI :Integer;
begin
     Result := _MaxI;
end;

procedure TDrawCopys1D.SetMaxI( const MaxI_:Integer );
begin
     _MaxI := MaxI_;  PosesN := MaxI - MinI + 1;
end;

//------------------------------------------------------------------------------

function TDrawCopys1D.GetPosYs( const I_:Integer ) :Single;
begin
     Result := Poses[ I_ - MinI ].Y;
end;

procedure TDrawCopys1D.SetPosYs( const I_:Integer; const PosYs_:Single );
begin
     Poses[ I_ - MinI ] := TSingle2D.Create( I_, PosYs_ );
end;

/////////////////////////////////////////////////////////////////////// メソッド

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TDrawCopys1D.Create;
begin
     inherited;

end;

procedure TDrawCopys1D.AfterConstruction;
begin
     inherited;

     MinI := 0;
     MaxI := 0;
end;

destructor TDrawCopys1D.Destroy;
begin

     inherited;
end;

/////////////////////////////////////////////////////////////////////// メソッド

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TDrawPoins1D

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

/////////////////////////////////////////////////////////////////////// アクセス

/////////////////////////////////////////////////////////////////////// メソッド

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TDrawPoins1D.Create;
begin
     inherited;

     _Poin := TDrawCirc.Create( Self );
end;

procedure TDrawPoins1D.AfterConstruction;
begin
     inherited;

     Poin.Radius := 0.1;
end;

destructor TDrawPoins1D.Destroy;
begin

     inherited;
end;

/////////////////////////////////////////////////////////////////////// メソッド

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TDrawLines1D

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

/////////////////////////////////////////////////////////////////////// アクセス

function TDrawLines1D.GetPosYs( const I_:Integer ) :Single;
begin
     Result := Poins[ I_ - MinI ].Y;
end;

procedure TDrawLines1D.SetPosYs( const I_:Integer; const PosYs_:Single );
begin
     Poins[ I_ - MinI ] := TSingle2D.Create( I_, PosYs_ );
end;

//------------------------------------------------------------------------------

function TDrawLines1D.GetMinI :Integer;
begin
     Result := _MinX;
end;

procedure TDrawLines1D.SetMinI( const MinI_:Integer );
begin
     _MinX := MinI_;  PoinsN := MaxI - MinI + 1;
end;

function TDrawLines1D.GetMaxI :Integer;
begin
     Result := _MaxX;
end;

procedure TDrawLines1D.SetMaxI( const MaxI_:Integer );
begin
     _MaxX := MaxI_;  PoinsN := MaxI - MinI + 1;
end;

/////////////////////////////////////////////////////////////////////// メソッド

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

/////////////////////////////////////////////////////////////////////// メソッド

procedure TDrawLines1D.Func( const Func_:TConstFunc<Integer,Single> );
var
   I :Integer;
begin
     for I := MinI to MaxI do PosYs[ I ] := Func_( I );
end;

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TDrawChain1D

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

/////////////////////////////////////////////////////////////////////// アクセス

function TDrawChain1D.GetPosYs( const I_:Integer ) :Single;
begin
     Result := _Poins.PosYs[ I_ ];
end;

procedure TDrawChain1D.SetPosYs( const I_:Integer; const PosYs_:Single );
begin
     _Poins.PosYs[ I_ ] := PosYs_;
     _Lines.PosYs[ I_ ] := PosYs_;
end;

//------------------------------------------------------------------------------

function TDrawChain1D.GetMinI :Integer;
begin
     Result := _Poins.MinI;
end;

procedure TDrawChain1D.SetMinI( const MinI_:Integer );
begin
     _Poins.MinI := MinI_;
     _Lines.MinI := MinI_;
end;

function TDrawChain1D.GetMaxI :Integer;
begin
     Result := _Poins.MaxI;
end;

procedure TDrawChain1D.SetMaxI( const MaxI_:Integer );
begin
     _Poins.MaxI := MaxI_;
     _Lines.MaxI := MaxI_;
end;

/////////////////////////////////////////////////////////////////////// メソッド

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TDrawChain1D.Create;
begin
     inherited;

     _Poins := TDrawPoins1D.Create( Self );
     _Lines := TDrawLines1D.Create( Self );
end;

procedure TDrawChain1D.AfterConstruction;
begin
     inherited;

end;

destructor TDrawChain1D.Destroy;
begin

     inherited;
end;

/////////////////////////////////////////////////////////////////////// メソッド

end. //######################################################################### ■
