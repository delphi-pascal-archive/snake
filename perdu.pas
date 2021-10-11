unit perdu;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls;

type
  TForm2 = class(TForm)
    Image1: TImage;
    Timer1: TTimer;

  procedure closer(sender:Tobject );
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  Form2: TForm2;


implementation

{$R *.dfm}
procedure TForm2.closer(sender: Tobject);
begin
  Timer1.Enabled:=false;
  Form2.visible:=false;
end;

end.
