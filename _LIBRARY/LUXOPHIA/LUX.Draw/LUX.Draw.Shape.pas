unit LUX.Draw.Shape;

interface //#################################################################### ■

uses System.Types, System.UITypes,
     FMX.Controls, FMX.Graphics,
     LUX, LUX.D1, LUX.D2,
     LUX.FMX.Graphics,
     LUX.Draw.Scene,
     LUX.Draw.Viewer;

type //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【型】

     TDrawCircle = class;
     TDrawCurve = class;

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TDrawCircle

     TDrawCircle = class( TDrawShape )
     private
     protected
       _Radius :Single;
       _Border :Single;
       ///// アクセス
       function GetRadius :Single;
       procedure SetRadius( const Radius_:Single );
       function GetBorder :Single;
       procedure SetBorder( const Border_:Single );
       ///// メソッド
       procedure DrawMain( const Canvas_:TCanvas ); override;
     public
       constructor Create; override;
       destructor Destroy; override;
       ///// プロパティ
       property Radius :Single read GetRadius write SetRadius;
       property Border :Single read GetBorder write SetBorder;
     end;

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TDrawCurve

     TDrawCurve = class( TDrawShape )
     private
       _Path :TPathData;
     protected
       _Poins   :TArray<TSingle2D>;
       _PoinsN  :Integer;
       ///// アクセス
       function GetPoins( const I_:Integer ) :TSingle2D;
       procedure SetPoins( const I_:Integer; const Value_:TSingle2D );
       function GetPoinsN :Integer;
       procedure SetPoinsN( const ValuesN_:Integer );
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

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TDrawCircle

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

/////////////////////////////////////////////////////////////////////// アクセス

function TDrawCircle.GetRadius :Single;
begin
     Result := _Radius;
end;

procedure TDrawCircle.SetRadius( const Radius_:Single );
begin
     _Radius := Radius_;
end;

function TDrawCircle.GetBorder :Single;
begin
     Result := _Border;
end;

procedure TDrawCircle.SetBorder( const Border_:Single );
begin
     _Border := Border_;
end;

/////////////////////////////////////////////////////////////////////// メソッド

procedure TDrawCircle.DrawMain( const Canvas_:TCanvas );
var
   R :TRectF;
begin
     inherited;

     R := TRectF.Empty;
     R.Inflate( _Radius, _Radius );

     Canvas_.FillEllipse( R, _Opacity );
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TDrawCircle.Create;
begin
     inherited;

     _Radius := 5;
     _Border := 0;
end;

destructor TDrawCircle.Destroy;
begin

     inherited;
end;

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TDrawCurve

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

/////////////////////////////////////////////////////////////////////// アクセス

function TDrawCurve.GetPoins( const I_:Integer ) :TSingle2D;
begin
     Result := _Poins[ I_ ];
end;

procedure TDrawCurve.SetPoins( const I_:Integer; const Value_:TSingle2D );
begin
     _Poins[ I_ ] := Value_;
end;

function TDrawCurve.GetPoinsN :Integer;
begin
     Result := _PoinsN;
end;

procedure TDrawCurve.SetPoinsN( const ValuesN_:Integer );
begin
     _PoinsN := ValuesN_;

     SetLength( _Poins, _PoinsN );
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TDrawCurve.Create;
begin
     inherited;

     _Path := TPathData.Create;

     PoinsN := 100;
end;

destructor TDrawCurve.Destroy;
begin
     _Path.DisposeOf;

     inherited;
end;

/////////////////////////////////////////////////////////////////////// メソッド

procedure TDrawCurve.DrawMain( const Canvas_:TCanvas );
var
   I :Integer;
begin
     inherited;

     with _Path do
     begin
          Clear;

          MoveTo( _Poins[ 0 ] );

          for I := 1 to _PoinsN-1 do LineTo( _Poins[ I ] );
     end;

     Canvas_.DrawPath( _Path, _Opacity );
end;

end. //######################################################################### ■
