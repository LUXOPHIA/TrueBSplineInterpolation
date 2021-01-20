unit LUX.Draw.Shape;

interface //#################################################################### ■

uses
     FMX.Controls, FMX.Graphics,
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
       destructor Destroy; override;
       ///// プロパティ
       property Radius :Single read GetRadius write SetRadius;
     end;

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TDrawCurv

     TDrawCurv = class( TDrawShape )
     private
       _Path :TPathData;
     protected
       _Poins   :TArray<TSingle2D>;  upPoins :Boolean;
       ///// アクセス
       function GetPoins( const I_:Integer ) :TSingle2D;
       procedure SetPoins( const I_:Integer; const Value_:TSingle2D );
       function GetPoinsN :Integer;
       procedure SetPoinsN( const PoinsN_:Integer );
       ///// メソッド
       procedure DrawMain( const Canvas_:TCanvas ); override;
     public
       constructor Create; override;
       destructor Destroy; override;
       ///// プロパティ
       property Poins[ const I_:Integer ] :TSingle2D read GetPoins  write SetPoins ; default;
       property PoinsN                    :Integer   read GetPoinsN write SetPoinsN;
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
     Result := Max( SizeX, SizeY ) / 2;
end;

procedure TDrawCirc.SetRadius( const Radius_:Single );
begin
     SizeX := 2 * Radius_;
     SizeY := 2 * Radius_;
end;

/////////////////////////////////////////////////////////////////////// メソッド

procedure TDrawCirc.DrawMain( const Canvas_:TCanvas );
begin
     inherited;

     Canvas_.FillEllipse( Area, _Opacity );

     if Assigned( _Stroke ) and ( _Stroke.Thickness > 0 ) then Canvas_.DrawEllipse( Area, _Opacity );
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TDrawCirc.Create;
begin
     inherited;

     Radius := 1;
end;

destructor TDrawCirc.Destroy;
begin

     inherited;
end;

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

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TDrawCurv.Create;
begin
     inherited;

     _Path := TPathData.Create;

     PoinsN := 100;
end;

destructor TDrawCurv.Destroy;
begin
     _Path.DisposeOf;

     inherited;
end;

/////////////////////////////////////////////////////////////////////// メソッド

procedure TDrawCurv.DrawMain( const Canvas_:TCanvas );
var
   I :Integer;
begin
     inherited;

     if upPoins then
     begin
          _Area := TSingleArea2D.NeInf;

          _Area.Add( _Poins );

          upPoins := False;
     end;

     with _Path do
     begin
          Clear;

          MoveTo( _Poins[ 0 ] );

          for I := 1 to PoinsN-1 do LineTo( _Poins[ I ] );
     end;

     Canvas_.DrawPath( _Path, _Opacity );
end;

end. //######################################################################### ■
