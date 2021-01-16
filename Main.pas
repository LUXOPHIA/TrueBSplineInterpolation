unit Main;

interface //####################################################################

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  LUX.Draw.Scene,
  LUX.Draw.Shape, LUX.Draw.Shape.Chart,
  LUX.Draw.Viewer;

type
  TForm1 = class(TForm)
    DrawViewer1: TDrawViewer;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { private 宣言 }
  public
    { public 宣言 }
    _Scene  :TDrawScene;
    _Camera :TDrawCamera;
    _Grid   :TDrawPlots;
    procedure MakeScene;
  end;

var
  Form1: TForm1;

implementation //###############################################################

{$R *.fmx}

procedure TForm1.MakeScene;
begin
     _Grid := TDrawPlots.Create( _Scene );
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

procedure TForm1.FormCreate(Sender: TObject);
begin
     _Scene := TDrawScene.Create;

     _Camera := TDrawCamera.Create( _Scene );

     MakeScene;

     DrawViewer1.Camera := _Camera;

     _Camera.Position := TPointF.Create( 2, 5 );
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
     _Scene.Free;
end;

////////////////////////////////////////////////////////////////////////////////

end. //#########################################################################
