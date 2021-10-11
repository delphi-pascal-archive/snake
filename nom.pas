unit nom;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,regles,perdu,snake,hiscore;

type
  TForm7 = class(TForm)
    Label1: TLabel;
    Edit1: TEdit;
    Button3: TButton;
    procedure Button3Click(Sender: TObject);

  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  Form7: TForm7;

implementation

{$R *.dfm}
//................................................
procedure TForm7.Button3Click(Sender: TObject);
begin
 Form7.Close;
 Form4.Visible:=true; 
 Form4.incrementer(pomme);
end;

end.
