unit LUX.Draw.Shape;

interface //#################################################################### ■

uses FMX.Graphics,
     LUX, LUX.D1, LUX.D2,
     LUX.Draw.Scene,
     LUX.Draw.Viewer;

type //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【型】

     TDrawCirc = class;
     TDrawCurv = class;

     //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【クラス】

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TDrawCirc

     TDrawCirc = class( TDrawShape )
     private
     protected
       ///// アクセス
       function GetRadius :Single;
       procedure SetRadius( const Radius_:Single );
       ///// メソッド
       procedure DrawMain( const Canvas_:TCanvas ); override;
     public
       constructor Create; override;
       procedure AfterConstruction; override;
       destructor Destroy; override;
       ///// プロパティ
       property Radius :Single read GetRadius write SetRadius;
       ///// メソッド
     end;

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TDrawCurv

     TDrawCurv = class( TDrawShape )
     private
       _Path :TPathData;
     protected
       _Poins :TArray<TSingle2D>;  upPoins :Boolean;
       ///// アクセス
       function GetPoins( const I_:Integer ) :TSingle2D;
       procedure SetPoins( const I_:Integer; const Value_:TSingle2D );
       function GetPoinsN :Integer;
       procedure SetPoinsN( const PoinsN_:Integer );
       ///// メソッド
       procedure DrawMain( const Canvas_:TCanvas ); override;
     public
       constructor Create; override;
       procedure AfterConstruction; override;
       destructor Destroy; override;
       ///// プロパティ
       property Poins[ const I_:Integer ] :TSingle2D read GetPoins  write SetPoins ; default;
       property PoinsN                    :Integer   read GetPoinsN write SetPoinsN;
       ///// メソッド
     end;

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TDrawCurv1D

     TDrawCurv1D = class( TDrawCurv )
     private
     protected
       _MinX :Single;
       _MaxX :Single;
       ///// アクセス
       function GetMinX :Single;
       procedure SetMinX( const MinX_:Single );
       function GetMaxX :Single;
       procedure SetMaxX( const MaxX_:Single );
       function GetDivN :Integer;
       procedure SetDivN( const DivN_:Integer );
       ///// メソッド
     public
       ///// プロパティ
       property MinX :Single  read GetMinX write SetMinX;
       property MaxX :Single  read GetMaxX write SetMaxX;
       property DivN :Integer read GetDivN write SetDivN;
       ///// メソッド
       procedure Func( const Func_:TConstFunc<Single,Single> );
     end;

implementation //############################################################### ■

uses System.Math;

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【クラス】

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TDrawCirc

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

/////////////////////////////////////////////////////////////////////// アクセス

function TDrawCirc.GetRadius :Single;
begin
     Result := Max( Area.SizeX, Area.SizeY ) / 2;
end;

procedure TDrawCirc.SetRadius( const Radius_:Single );
var
   A :TSingleArea2D;
begin
     A := Area;

     A.SizeX := 2 * Radius_;
     A.SizeY := 2 * Radius_;

     Area := A;
end;

/////////////////////////////////////////////////////////////////////// メソッド

procedure TDrawCirc.DrawMain( const Canvas_:TCanvas );
begin
     inherited;

     Canvas_.FillEllipse( Area, _Opacity );

     if Stroke.Kind <> TBrushKind.None then Canvas_.DrawEllipse( Area, _Opacity );
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TDrawCirc.Create;
begin
     inherited;

end;

procedure TDrawCirc.AfterConstruction;
begin
     inherited;

     Radius := 1;
end;

destructor TDrawCirc.Destroy;
begin

     inherited;
end;

/////////////////////////////////////////////////////////////////////// メソッド

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TDrawCurv

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

/////////////////////////////////////////////////////////////////////// アクセス

function TDrawCurv.GetPoins( const I_:Integer ) :TSingle2D;
begin
     Result := _Poins[ I_ ];
end;

procedure TDrawCurv.SetPoins( const I_:Integer; const Value_:TSingle2D );
begin
     _Poins[ I_ ] := Value_;  upPoins := True;
end;

function TDrawCurv.GetPoinsN :Integer;
begin
     Result := Length( _Poins );
end;

procedure TDrawCurv.SetPoinsN( const PoinsN_:Integer );
begin
     SetLength( _Poins, PoinsN_ );  upPoins := True;
end;

/////////////////////////////////////////////////////////////////////// メソッド

procedure TDrawCurv.DrawMain( const Canvas_:TCanvas );
var
   I :Integer;
begin
     inherited;

     if upPoins then
     begin
          _RelaArea := TSingleArea2D.NeInf;

          _RelaArea.Add( _Poins );

          with _Path do
          begin
               Clear;

               MoveTo( _Poins[ 0 ] );

               for I := 1 to PoinsN-1 do LineTo( _Poins[ I ] );
          end;

          upPoins := False;
     end;

     Canvas_.DrawPath( _Path, _Opacity );
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TDrawCurv.Create;
begin
     inherited;

     _Path := TPathData.Create;
end;

procedure TDrawCurv.AfterConstruction;
begin
     inherited;

     PoinsN := 100;
end;

destructor TDrawCurv.Destroy;
begin
     _Path.DisposeOf;

     inherited;
end;

/////////////////////////////////////////////////////////////////////// メソッド

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TDrawCurv1D

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

/////////////////////////////////////////////////////////////////////// アクセス

function TDrawCurv1D.GetMinX :Single;
begin
     Result := _MinX;
end;

procedure TDrawCurv1D.SetMinX( const MinX_:Single );
begin
     _MinX := MinX_;
end;

function TDrawCurv1D.GetMaxX :Single;
begin
     Result := _MaxX;
end;

procedure TDrawCurv1D.SetMaxX( const MaxX_:Single );
begin
     _MaxX := MaxX_;
end;

//------------------------------------------------------------------------------

function TDrawCurv1D.GetDivN :Integer;
begin
     Result := PoinsN - 1;
end;

procedure TDrawCurv1D.SetDivN( const DivN_:Integer );
begin
     PoinsN := DivN_ + 1;
end;

/////////////////////////////////////////////////////////////////////// メソッド

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

/////////////////////////////////////////////////////////////////////// メソッド

procedure TDrawCurv1D.Func( const Func_:TConstFunc<Single,Single> );
var
   I :Integer;
   X :Single;
begin
     for I := 0 to DivN do
     begin
          X := ( MaxX - MinX ) * I / DivN + MinX;

          Poins[ I ] := TSingle2D.Create( X, Func_( X ) );
     end;
end;

end. //######################################################################### ■
