unit Main;

interface //#################################################################### ■

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  CurveChartFrame, LUX.Draw.Viewer, FMX.Controls.Presentation, FMX.StdCtrls;

type
  TForm1 = class(TForm)
    CurveChart1: TCurveChart;
    Panel1: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { private 宣言 }
  public
    { public 宣言 }
  end;

var
  Form1: TForm1;

implementation //############################################################### ■

{$R *.fmx}

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

procedure TForm1.FormCreate(Sender: TObject);
begin
     /////
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
     /////
end;

////////////////////////////////////////////////////////////////////////////////

end. //######################################################################### ■
