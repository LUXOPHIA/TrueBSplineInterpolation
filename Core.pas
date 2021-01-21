unit Core;

interface //#################################################################### ■

uses LUX;

type //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【型】

     //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【レコード】

     //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【クラス】

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TBSInterp

     TBSInterp = class
     private
       procedure MakePoins;
     protected
       _Poins  :TArray<Single>;  upPoins :Boolean;
       _Conts  :TArray<Single>;
       _PoinsN :Integer;
       _MargsN :Integer;
       ///// アクセス
       function GetPoins( const I_:Integer ) :Single;
       procedure SetPoins( const I_:Integer; const Poins_:Single );
       function GetPoinsN :Integer;
       procedure SetPoinsN( const PoinsN_:Integer );
       function GetMargsN :Integer;
       procedure SetMargsN( const MargsN_:Integer );
       ///// メソッド
       procedure ExtePoins;
       procedure MakeConts;
     public
       constructor Create;
       destructor Destroy; override;
       ///// プロパティ
       property Poins[ const I_:Integer ] :Single  read GetPoins  write SetPoins ;
       property PoinsN                    :Integer read GetPoinsN write SetPoinsN;
       property MargsN                    :Integer read GetMargsN write SetMargsN;
       ///// メソッド
       function Interp( const X_:Single ) :Single;
     end;

//const //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【定数】

//var //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【変数】

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【ルーチン】

implementation //############################################################### ■

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【レコード】

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【クラス】

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TBSInterp

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

procedure TBSInterp.MakePoins;
begin
     SetLength( _Poins, _MargsN + _PoinsN + _MargsN );  upPoins := True;
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

function TBSInterp.GetPoins( const I_:Integer ) :Single;
begin
     Result := _Poins[ _MargsN + I_ ];
end;

procedure TBSInterp.SetPoins( const I_:Integer; const Poins_:Single );
begin
     _Poins[ _MargsN + I_ ] := Poins_;  upPoins := True;
end;

//------------------------------------------------------------------------------

function TBSInterp.GetPoinsN :Integer;
begin
     Result := _PoinsN;
end;

procedure TBSInterp.SetPoinsN( const PoinsN_:Integer );
begin
     _PoinsN := PoinsN_;  MakePoins;
end;

//------------------------------------------------------------------------------

function TBSInterp.GetMargsN :Integer;
begin
     Result := _MargsN;
end;

procedure TBSInterp.SetMargsN( const MargsN_:Integer );
begin
     _MargsN := MargsN_;  MakePoins;
end;

/////////////////////////////////////////////////////////////////////// メソッド

procedure TBSInterp.ExtePoins;
begin

end;

procedure TBSInterp.MakeConts;
begin

end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TBSInterp.Create;
begin
     inherited;

     PoinsN := 8;
     MargsN := 8;
end;

destructor TBSInterp.Destroy;
begin

     inherited;
end;

/////////////////////////////////////////////////////////////////////// メソッド

function TBSInterp.Interp( const X_:Single ) :Single;
begin
     if upPoins then
     begin
          ExtePoins;
          MakeConts;

          upPoins := False;
     end;
end;

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【ルーチン】

//############################################################################## □

initialization //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ 初期化

finalization //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ 最終化

end. //######################################################################### ■