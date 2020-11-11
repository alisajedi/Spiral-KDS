unit Unit2;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Unit1;

type
  TForm2 = class(TForm)
    Edit1: TEdit;
    Label1: TLabel;
    Edit2: TEdit;
    Label2: TLabel;
    Edit3: TEdit;
    Label3: TLabel;
    Edit4: TEdit;
    Label4: TLabel;
    Edit5: TEdit;
    Label5: TLabel;
    Edit6: TEdit;
    Label6: TLabel;
    Edit7: TEdit;
    Label7: TLabel;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Label8: TLabel;
    Edit8: TEdit;
    Label9: TLabel;
    Edit9: TEdit;
    procedure FormCreate(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Edit2Change(Sender: TObject);
    procedure Edit3Change(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure Button5Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

uses Math;

{$R *.dfm}

procedure TForm2.FormCreate(Sender: TObject);
begin
  Button4Click(self);
end;

procedure TForm2.Button2Click(Sender: TObject);
begin //default:
  edit1.text:= intToStr(DEFAULT_POINTS_NUMBER);;
  edit2.text:= intToStr(DEFAULT_PB_WIDTH);
  edit3.text:= intToStr(DEFAULT_PB_HEIGHT);
  edit4.text:= intToStr(DEFAULT_FORM_WIDTH);
  edit5.text:= intToStr(DEFAULT_FORM_HEIGHT);
  edit6.text:= intToStr(DEFAULT_MAX_PAINTBOX_HEIGHT);
  edit7.text:= intToStr(DEFAULT_MAX_PAINTBOX_WIDTH);
  edit8.Text:= intToStr(DEFAULT_OVERHEAD_QUEUE_NUMBER);
  edit9.Text:= floatToStr(form1.calculateConvergencyAccuracy(DEFAULT_POINTS_NUMBER)); 
end;

procedure TForm2.Button1Click(Sender: TObject);
var
  w: Word;
  shs: TShiftState;
begin //Apply & start:
  form1.pointsNumber:= strToInt(edit1.Text);
  form1.pbWidth:= strToInt(edit2.Text);
  form1.pbHeight:= strToInt(edit3.Text);
  form1.formWidth:= strToInt(edit4.Text);
  form1.formHeight:= strToInt(edit5.Text);
  form1.maxPaintBoxWidth:= strToInt(edit6.Text);
  form1.maxPaintBoxHeight:= strToInt(edit7.Text);
  form1.convergencyQueueNumber:= strToInt(edit8.Text);
  form1.convergencyAccuracy:= strToFloat(edit9.Text);
  w:= ord('1');
  shs:= [];
  form1.FormKeyDown(self, w, shs);
end;

procedure TForm2.Button3Click(Sender: TObject);
begin
  form1.convergencyQueueNumber:= strToInt(edit8.Text);
  form1.convergencyAccuracy:= strToFloat(edit9.Text);
  close;
end;

procedure TForm2.Button4Click(Sender: TObject);
begin //Read from form:
  edit1.Text:= intToStr(form1.pointsNumber);
  edit2.Text:= intToStr(form1.pbWidth);
  edit3.Text:= intToStr(form1.pbHeight);
  edit4.Text:= intToStr(form1.formWidth);
  edit5.Text:= intToStr(form1.formHeight);
  edit6.Text:= intToStr(form1.maxPaintBoxWidth);
  edit7.Text:= intToStr(form1.maxPaintBoxHeight);
  edit8.Text:= intToStr(form1.convergencyQueueNumber);
  edit9.Text:= floatToStr(form1.convergencyAccuracy);
end;

procedure TForm2.Edit2Change(Sender: TObject);
begin
  if edit2.Text <> '' then
    if strToInt(edit2.Text) + 24 < 1024 then
      edit4.Text:= intToStr(strToInt(edit2.Text) + 24)
    else
      edit4.Text:= '1024';
end;

procedure TForm2.Edit3Change(Sender: TObject);
begin
  if edit3.Text <> '' then
    if strToInt(edit3.text) + 49 < 768 then
      edit5.text:= intToStr(strToInt(edit3.text) + 49)
    else
      edit5.text:= '768';
end;

procedure TForm2.Edit1Change(Sender: TObject);
begin
  if edit1.Text <> '' then
    begin
    edit2.text:= intToStr(min(430, 170*round(sqrt(strToInt(edit1.text)))));
    edit3.text:= intToStr(min(430, strToInt(edit2.Text) + TEXT_HEIGHT));
{   //For large N, this method causes multiple events in one attempt:
    edit2.text:= intToStr(round(170*power(strToInt(edit1.text), 0.2)));
    edit3.text:= intToStr(strToInt(edit2.Text) + TEXT_HEIGHT);
}
    end;
end;

procedure TForm2.FormKeyPress(Sender: TObject; var Key: Char);
begin
  case key of
    chr(27):
      Button3Click(self);
    chr(13):
      begin
      Button5Click(self);
      close;
      end;
    end; //case key.
end;

procedure TForm2.Button5Click(Sender: TObject);
begin
  Button1Click(self);
  Button3Click(self);
end;

end.
