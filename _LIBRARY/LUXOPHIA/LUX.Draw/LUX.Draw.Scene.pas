unit LUX.Draw.Scene;

interface //#################################################################### ■

uses System.UITypes, System.Math.Vectors,
     FMX.Graphics,
     LUX, LUX.D1, LUX.D2, LUX.Data.Tree;

type //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【型】

     TDrawNode           = class;
       TDrawRoot         = class;
         TDrawScene      = class;
       TDrawKnot         = class;
         TDrawShape      = class;
           TDrawCamera   = class;
           TDrawCopys    = class;
           TDrawPosCopys = class;

     //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【クラス】

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TDrawNode

     TDrawNode = class( TTreeNode<TDrawNode,TDrawNode> )
     private
     protected
       _RelaArea :TSingleArea2D;
       ///// アクセス
       function GetRelaArea :TSingleArea2D; virtual;
       procedure SetRelaArea( const RelaArea_:TSingleArea2D ); virtual;
       function GetRelaPose :TMatrix; virtual; abstract;
       procedure SetRelaPose( const RelaPose_:TMatrix ); virtual; abstract;
       function GetAbsoPose :TMatrix; virtual;
       procedure SetAbsoPose( const AbsoPose_:TMatrix ); virtual;
       function GetRelaPosi :TSingle2D; virtual;
       procedure SetRelaPosi( const RelaPosi_:TSingle2D ); virtual;
       function GetStroke :TStrokeBrush; virtual;
       procedure SetStroke( const Stroke_:TStrokeBrush ); virtual;
       function GetFiller :TBrush; virtual;
       procedure SetFiller( const Filler_:TBrush ); virtual;
       function GetVisible :Boolean; virtual;
       procedure SetVisible( const Visible_:Boolean ); virtual;
       ///// メソッド
       procedure DrawBegin( const Canvas_:TCanvas ); virtual;
       procedure DrawMain( const Canvas_:TCanvas ); virtual;
       procedure DrawEnd( const Canvas_:TCanvas ); virtual;
     public
       constructor Create; override;
       procedure AfterConstruction; override;
       destructor Destroy; override;
       ///// プロパティ
       property RelaArea :TSingleArea2D read GetRelaArea write SetRelaArea;
       property     Area :TSingleArea2D read GetRelaArea write SetRelaArea;
       property RelaPose :TMatrix       read GetRelaPose write SetRelaPose;
       property     Pose :TMatrix       read GetRelaPose write SetRelaPose;
       property RelaPosi :TSingle2D     read GetRelaPosi write SetRelaPosi;
       property Position :TSingle2D     read GetRelaPosi write SetRelaPosi;
       property AbsoPose :TMatrix       read GetAbsoPose write SetAbsoPose;
       property Stroke   :TStrokeBrush  read GetStroke   write SetStroke  ;
       property Filler   :TBrush        read GetFiller   write SetFiller  ;
       property Visible  :Boolean       read GetVisible  write SetVisible ;
       ///// メソッド
       procedure Draw( const Canvas_:TCanvas ); virtual;
     end;

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TDrawRoot

     TDrawRoot = class( TDrawNode )
     private
     protected
       ///// アクセス
       function GetStroke :TStrokeBrush; override;
       function GetFiller :TBrush; override;
       function GetRelaPose :TMatrix; override;
       procedure SetRelaPose( const RelaPose_:TMatrix ); override;
       function GetAbsoPose :TMatrix; override;
       procedure SetAbsoPose( const AbsoPose_:TMatrix ); override;
       ///// メソッド
     public
       constructor Create; override;
       procedure AfterConstruction; override;
       destructor Destroy; override;
       ///// プロパティ
       property RelaPose :TMatrix   read GetRelaPose;
       property     Pose :TMatrix   read GetRelaPose;
       property RelaPosi :TSingle2D read GetRelaPosi;
       property Position :TSingle2D read GetRelaPosi;
       property AbsoPose :TMatrix   read GetAbsoPose;
       ///// メソッド
     end;

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TDrawScene

     TDrawScene = class( TDrawRoot )
     private
     protected
       _Stroke    :TStrokeBrush;
       _Filler    :TBrush;
       _BackColor :TAlphaColor;
       ///// アクセス
       function GetStroke :TStrokeBrush; override;
       procedure SetStroke( const Stroke_:TStrokeBrush ); override;
       function GetFiller :TBrush; override;
       procedure SetFiller( const Filler_:TBrush ); override;
       function GetBackColor :TAlphaColor;
       procedure SetBackColor( const BackColor_:TAlphaColor );
       ///// メソッド
       procedure DrawBegin( const Canvas_:TCanvas ); override;
       procedure DrawMain( const Canvas_:TCanvas ); override;
     public
       constructor Create; override;
       procedure AfterConstruction; override;
       destructor Destroy; override;
       ///// プロパティ
       property Stroke    :TStrokeBrush read GetStroke                      ;
       property Filler    :TBrush       read GetFiller                      ;
       property BackColor :TAlphaColor  read GetBackColor write SetBackColor;
       ///// メソッド
     end;

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TDrawKnot

     TDrawKnot = class( TDrawNode )
     private
       _State :TCanvasSaveState;
     protected
       _Opacity :Single;
       _Stroke  :TStrokeBrush;
       _Filler  :TBrush;
       _Visible :Boolean;
       ///// アクセス
       function GetOpacity :Single; virtual;
       procedure SetOpacity( const Opacity_:Single ); virtual;
       function GetStroke :TStrokeBrush; override;
       procedure SetStroke( const Stroke_:TStrokeBrush ); override;
       function GetFiller :TBrush; override;
       procedure SetFiller( const Filler_:TBrush ); override;
       function GetVisible :Boolean; override;
       procedure SetVisible( const Visible_:Boolean ); override;
       ///// メソッド
       procedure DrawBegin( const Canvas_:TCanvas ); override;
     public
       constructor Create; override;
       procedure AfterConstruction; override;
       destructor Destroy; override;
       ///// プロパティ
       property Opacity :Single read GetOpacity write SetOpacity;
       ///// メソッド
       procedure Draw( const Canvas_:TCanvas ); override;
     end;

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TDrawShape

     TDrawShape = class( TDrawKnot )
     private
     protected
       _RelaPose :TMatrix;
       ///// アクセス
       function GetRelaPose :TMatrix; override;
       procedure SetRelaPose( const RelaPose_:TMatrix ); override;
       ///// メソッド
     public
       constructor Create; override;
       procedure AfterConstruction; override;
       destructor Destroy; override;
       ///// プロパティ
       ///// メソッド
     end;

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TDrawCamera

     TDrawCamera = class( TDrawShape )
     private
     protected
       ///// アクセス
       ///// メソッド
     public
       constructor Create; override;
       procedure AfterConstruction; override;
       destructor Destroy; override;
       ///// プロパティ
       ///// メソッド
       procedure Render( const Canvas_:TCanvas );
     end;

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TDrawCopys

     TDrawCopys = class( TDrawShape )
     private
     protected
       _Poses :TArray<TMatrix>;
       ///// アクセス
       function GetPoses( const I_:Integer ) :TMatrix;
       procedure SetPoses( const I_:Integer; const Poses_:TMatrix );
       function GetPosesN :Integer;
       procedure SetPosesN( const PosesN_:Integer );
       ///// メソッド
       procedure DrawMain( const Canvas_:TCanvas ); override;
     public
       constructor Create; override;
       procedure AfterConstruction; override;
       destructor Destroy; override;
       ///// プロパティ
       property Poses[ const I_:Integer ] :TMatrix read GetPoses  write SetPoses ;
       property PosesN                    :Integer read GetPosesN write SetPosesN;
       ///// メソッド
     end;

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TDrawPosCopys

     TDrawPosCopys = class( TDrawShape )
     private
     protected
       _Poses :TArray<TSingle2D>;
       ///// アクセス
       function GetPoses( const I_:Integer ) :TSingle2D;
       procedure SetPoses( const I_:Integer; const Poses_:TSingle2D );
       function GetPosesN :Integer;
       procedure SetPosesN( const PosesN_:Integer );
       ///// メソッド
       procedure DrawMain( const Canvas_:TCanvas ); override;
     public
       constructor Create; override;
       procedure AfterConstruction; override;
       destructor Destroy; override;
       ///// プロパティ
       property Poses[ const I_:Integer ] :TSingle2D read GetPoses  write SetPoses ;
       property PosesN                    :Integer   read GetPosesN write SetPosesN;
       ///// メソッド
     end;

implementation //############################################################### ■

uses System.Math;

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【クラス】

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TDrawNode

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

/////////////////////////////////////////////////////////////////////// アクセス

function TDrawNode.GetRelaArea :TSingleArea2D;
begin
     Result := _RelaArea;
end;

procedure TDrawNode.SetRelaArea( const RelaArea_:TSingleArea2D );
begin
     _RelaArea := RelaArea_;
end;

//------------------------------------------------------------------------------

function TDrawNode.GetAbsoPose :TMatrix;
begin
     Result := Parent.AbsoPose * RelaPose;
end;

procedure TDrawNode.SetAbsoPose( const AbsoPose_:TMatrix );
begin
     RelaPose := Parent.AbsoPose.Inverse * AbsoPose_;
end;

//------------------------------------------------------------------------------

function TDrawNode.GetRelaPosi :TSingle2D;
begin
     Result.X := RelaPose.m31;
     Result.Y := RelaPose.m32;
end;

procedure TDrawNode.SetRelaPosi( const RelaPosi_:TSingle2D );
var
   M :TMatrix;
begin
     M := RelaPose;

     M.m31 := RelaPosi_.X;
     M.m32 := RelaPosi_.Y;

     RelaPose := M;
end;

//------------------------------------------------------------------------------

function TDrawNode.GetStroke :TStrokeBrush;
begin
     Result := nil;
end;

procedure TDrawNode.SetStroke( const Stroke_:TStrokeBrush );
begin

end;

//------------------------------------------------------------------------------

function TDrawNode.GetFiller :TBrush;
begin
     Result := nil;
end;

procedure TDrawNode.SetFiller( const Filler_:TBrush );
begin

end;

//------------------------------------------------------------------------------

function TDrawNode.GetVisible :Boolean;
begin
     Result := True;
end;

procedure TDrawNode.SetVisible( const Visible_:Boolean );
begin

end;

/////////////////////////////////////////////////////////////////////// メソッド

procedure TDrawNode.DrawBegin( const Canvas_:TCanvas );
begin

end;

procedure TDrawNode.DrawMain( const Canvas_:TCanvas );
begin

end;

procedure TDrawNode.DrawEnd( const Canvas_:TCanvas );
var
   I :Integer;
begin
     for I := 0 to ChildsN-1 do Childs[ I ].Draw( Canvas_ );
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TDrawNode.Create;
begin
     inherited;

end;

procedure TDrawNode.AfterConstruction;
begin
     inherited;

     Area := TSingleArea2D.Create( -1, -1, +1, +1 );
end;

destructor TDrawNode.Destroy;
begin

     inherited;
end;

/////////////////////////////////////////////////////////////////////// メソッド

procedure TDrawNode.Draw( const Canvas_:TCanvas );
begin
     if Visible then
     begin
          DrawBegin( Canvas_ );
          DrawMain ( Canvas_ );
          DrawEnd  ( Canvas_ );
     end;
end;

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TDrawRoot

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

/////////////////////////////////////////////////////////////////////// アクセス

function TDrawRoot.GetStroke :TStrokeBrush;
begin
     Result := nil;
end;

//------------------------------------------------------------------------------

function TDrawRoot.GetFiller :TBrush;
begin
     Result := nil;
end;

//------------------------------------------------------------------------------

function TDrawRoot.GetRelaPose :TMatrix;
begin
     Result := TMatrix.Identity;
end;

procedure TDrawRoot.SetRelaPose( const RelaPose_:TMatrix );
begin

end;

//------------------------------------------------------------------------------

function TDrawRoot.GetAbsoPose :TMatrix;
begin
     Result := RelaPose;
end;

procedure TDrawRoot.SetAbsoPose( const AbsoPose_:TMatrix );
begin

end;

/////////////////////////////////////////////////////////////////////// メソッド

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TDrawRoot.Create;
begin
     inherited;

end;

procedure TDrawRoot.AfterConstruction;
begin
     inherited;

end;

destructor TDrawRoot.Destroy;
begin

     inherited;
end;

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TDrawScene

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

/////////////////////////////////////////////////////////////////////// アクセス

function TDrawScene.GetStroke :TStrokeBrush;
begin
     Result := _Stroke;
end;

procedure TDrawScene.SetStroke( const Stroke_:TStrokeBrush );
begin

end;

//------------------------------------------------------------------------------

function TDrawScene.GetFiller :TBrush;
begin
     Result := _Filler;
end;

procedure TDrawScene.SetFiller( const Filler_:TBrush );
begin

end;

//------------------------------------------------------------------------------

function TDrawScene.GetBackColor :TAlphaColor;
begin
     Result := _BackColor;
end;

procedure TDrawScene.SetBackColor( const BackColor_:TAlphaColor );
begin
     _BackColor := BackColor_;
end;

/////////////////////////////////////////////////////////////////////// メソッド

procedure TDrawScene.DrawBegin( const Canvas_:TCanvas );
begin
     with Canvas_ do
     begin
          Stroke.Assign( _Stroke );
          Fill  .Assign( _Filler );
     end;
end;

procedure TDrawScene.DrawMain( const Canvas_:TCanvas );
begin
     inherited;

     Canvas_.Clear( _BackColor );
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TDrawScene.Create;
begin
     inherited;

     _Stroke := TStrokeBrush.Create( TBrushKind.Solid, TAlphaColors.Black );
     _Filler := TBrush      .Create( TBrushKind.Solid, TAlphaColors.White );
end;

procedure TDrawScene.AfterConstruction;
begin
     inherited;

     with Stroke do
     begin
          Join      := TStrokeJoin.Round;
          Thickness := 0.02;
     end;

     BackColor := TAlphaColors.White;
end;

destructor TDrawScene.Destroy;
begin
     _Stroke.Free;
     _Filler.Free;

     inherited;
end;

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TDrawKnot

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

/////////////////////////////////////////////////////////////////////// アクセス

function TDrawKnot.GetOpacity :Single;
begin
     Result := _Opacity;
end;

procedure TDrawKnot.SetOpacity( const Opacity_:Single );
begin
     _Opacity := Opacity_;
end;

//------------------------------------------------------------------------------

function TDrawKnot.GetStroke :TStrokeBrush;
begin
     if Assigned( _Stroke ) then Result :=       _Stroke
                            else Result := Parent.Stroke;
end;

procedure TDrawKnot.SetStroke( const Stroke_:TStrokeBrush );
begin
     _Stroke := Stroke_;
end;

//------------------------------------------------------------------------------

function TDrawKnot.GetFiller :TBrush;
begin
     if Assigned( _Filler ) then Result :=       _Filler
                            else Result := Parent.Filler;
end;

procedure TDrawKnot.SetFiller( const Filler_:TBrush );
begin
     _Filler := Filler_;
end;

//------------------------------------------------------------------------------

function TDrawKnot.GetVisible :Boolean;
begin
     Result := _Visible;
end;

procedure TDrawKnot.SetVisible( const Visible_:Boolean );
begin
     _Visible := Visible_;
end;

/////////////////////////////////////////////////////////////////////// メソッド

procedure TDrawKnot.DrawBegin( const Canvas_:TCanvas );
begin
     inherited;

     with Canvas_ do
     begin
          MultiplyMatrix( RelaPose );

          if Assigned( _Stroke ) then Stroke.Assign( _Stroke );
          if Assigned( _Filler ) then Fill  .Assign( _Filler );
     end;
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TDrawKnot.Create;
begin
     inherited;

     _State := TCanvasSaveState.Create;

     _Stroke := nil;
     _Filler := nil;
end;

procedure TDrawKnot.AfterConstruction;
begin
     inherited;

     Opacity := 1;
     Visible := True;
end;

destructor TDrawKnot.Destroy;
begin
     if Assigned( _Stroke ) then _Stroke.Free;
     if Assigned( _Filler ) then _Filler.Free;

     _State.Free;

     inherited;
end;

/////////////////////////////////////////////////////////////////////// メソッド

procedure TDrawKnot.Draw( const Canvas_:TCanvas );
begin
     _State.Assign( Canvas_ );

     inherited;

     Canvas_.Assign( _State );
end;

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TDrawShape

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

/////////////////////////////////////////////////////////////////////// アクセス

function TDrawShape.GetRelaPose :TMatrix;
begin
     Result := _RelaPose;
end;

procedure TDrawShape.SetRelaPose( const RelaPose_:TMatrix );
begin
     _RelaPose := RelaPose_;
end;

/////////////////////////////////////////////////////////////////////// メソッド

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TDrawShape.Create;
begin
     inherited;

end;

procedure TDrawShape.AfterConstruction;
begin
     inherited;

     RelaPose := TMatrix.Identity;
end;

destructor TDrawShape.Destroy;
begin

     inherited;
end;

/////////////////////////////////////////////////////////////////////// メソッド

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TDrawCamera

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

/////////////////////////////////////////////////////////////////////// アクセス

/////////////////////////////////////////////////////////////////////// メソッド

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TDrawCamera.Create;
begin
     inherited;

end;

procedure TDrawCamera.AfterConstruction;
begin
     inherited;

     Area := TSIngleArea2D.Create( -10, -10, +10, +10 );
end;

destructor TDrawCamera.Destroy;
begin

     inherited;
end;

/////////////////////////////////////////////////////////////////////// メソッド

procedure TDrawCamera.Render( const Canvas_:TCanvas );
begin
     Canvas_.MultiplyMatrix( AbsoPose.Inverse );

     ( Self.RootNode as TDrawScene ).Draw( Canvas_ );
end;

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TDrawCopys

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

/////////////////////////////////////////////////////////////////////// アクセス

function TDrawCopys.GetPoses( const I_:Integer ) :TMatrix;
begin
     Result := _Poses[ I_ ];
end;

procedure TDrawCopys.SetPoses( const I_:Integer; const Poses_:TMatrix );
begin
     _Poses[ I_ ] := Poses_;
end;

function TDrawCopys.GetPosesN :Integer;
begin
     Result := Length( _Poses );
end;

procedure TDrawCopys.SetPosesN( const PosesN_:Integer );
var
   I :Integer;
begin
     SetLength( _Poses, PosesN_ );

     for I := 0 to PosesN-1 do _Poses[ I ] := TMatrix.Identity;
end;

/////////////////////////////////////////////////////////////////////// メソッド

procedure TDrawCopys.DrawMain( const Canvas_:TCanvas );
var
   M :TMatrix;
   I :Integer;
begin
     inherited;

     M := Canvas_.Matrix;

     for I := 0 to PosesN-1 do
     begin
          Canvas_.SetMatrix( _Poses[ I ] * M );

          DrawEnd( Canvas_ );
     end;
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TDrawCopys.Create;
begin
     inherited;

end;

procedure TDrawCopys.AfterConstruction;
begin
     inherited;

     PosesN := 1;
end;

destructor TDrawCopys.Destroy;
begin

     inherited;
end;

/////////////////////////////////////////////////////////////////////// メソッド

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TDrawPosCopys

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

/////////////////////////////////////////////////////////////////////// アクセス

function TDrawPosCopys.GetPoses( const I_:Integer ) :TSingle2D;
begin
     Result := _Poses[ I_ ];
end;

procedure TDrawPosCopys.SetPoses( const I_:Integer; const Poses_:TSingle2D );
begin
     _Poses[ I_ ] := Poses_;
end;

function TDrawPosCopys.GetPosesN :Integer;
begin
     Result := Length( _Poses );
end;

procedure TDrawPosCopys.SetPosesN( const PosesN_:Integer );
begin
     SetLength( _Poses, PosesN_ );
end;

/////////////////////////////////////////////////////////////////////// メソッド

procedure TDrawPosCopys.DrawMain( const Canvas_:TCanvas );
var
   M :TMatrix;
   I :Integer;
begin
     inherited;

     M := Canvas_.Matrix;

     for I := 0 to PosesN-1 do
     begin
          with _Poses[ I ] do Canvas_.SetMatrix( TMatrix.CreateTranslation( X, Y ) * M );

          DrawEnd( Canvas_ );
     end;
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TDrawPosCopys.Create;
begin
     inherited;

end;

procedure TDrawPosCopys.AfterConstruction;
begin
     inherited;

     PosesN := 1;
end;

destructor TDrawPosCopys.Destroy;
begin

     inherited;
end;

end. //######################################################################### ■
