unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, XSBuiltIns, ComCtrls;

const
  TEXT_HEIGHT= 40;
  MAX_POINTS_NUMBER= 10000; //34000
  MAX_POINTS_NUMBER_TO_DRAW_SPIRAL= 300;
  DEFAULT_POINTS_NUMBER= 4;
  DEFAULT_PB_WIDTH= 224;
  DEFAULT_PB_HEIGHT= 224 + TEXT_HEIGHT;
  DEFAULT_FORM_WIDTH= DEFAULT_PB_WIDTH + 24;
  DEFAULT_FORM_HEIGHT= DEFAULT_PB_HEIGHT + 49;
  DEFAULT_MAX_PAINTBOX_HEIGHT= 350000 + TEXT_HEIGHT;
  DEFAULT_MAX_PAINTBOX_WIDTH= 350000;

  MAX_OVERHEAD_QUEUE_NUMBER= 20;
  DEFAULT_OVERHEAD_QUEUE_NUMBER= 10;
  CONVERGENCY_PARAMETER_IS_OVERHEAD= false;
  MINIMUM_EVENTS_TO_OCCUR_BEFORE_RESET= 21;

  UNKNOWN= -1;
type
  TSpiral = ^TPointRec;
//  TEventPointer = ^TEventRec;
  TPointRec= record
    next, prior, FC, LC: TSpiral; //firstChild,lastChild
    id : Integer;
    x, y, dx, dy: extended;
    //events: array[1..10] of TEventPointer;
  end;

{
  TEventRec= record
    next, before: TEventPointer;
    point: TSpiral;
  end;
}
  TEventType= (GI, GO, BE_FC, NOT_FC, BE_LC, NOT_LC, CD, RESIZE_PAINTBOX, EXTERNAL_EV, INTERNAL_EV, NOTHING);
  TRegionBound= 1..4;
  RegionDegree= record
    region: TRegionBound;
    degreeWithXCoords: Extended;
  end;
  TLogRecord= record
    totalAttemptNumber: Int64;
    totalOccurredEvents: Int64;
    occurredEvent: array[GI..INTERNAL_EV] of Int64;
    changedPointsAfterOccurrence: array[GI..INTERNAL_EV] of Int64;
  end;
  TOverheadLogRecord= Record
    externalOccurred, internalOccurred: Integer;
    externalChangedPointsAfterOccurrence, internalChangedPointsAfterOccurrence: Integer;
  end;
  TOverheadQueue= class
    private
      items: Array[1..MAX_OVERHEAD_QUEUE_NUMBER] of TOverheadLogRecord;
      first, last: Integer;
    public
      constructor create();
      procedure enqueue(item: TOverheadLogRecord); overload;
      procedure enqueue(externalOccurred, internalOccurred, externalChangedPointsAfterOccurrence, internalChangedPointsAfterOccurrence: Integer); overload;
      function dequeue(): TOverheadLogRecord;
      function isValidWithAccuracy():Boolean;
  end;

  TForm1 = class(TForm)
    pb: TPaintBox;
    Timer1: TTimer;
    Label1: TLabel;
    Label2: TLabel;

    procedure calculateNewCoordinates(p:TSpiral);
    procedure clearSpiral(p:TSpiral);
    function convertToCorrectSpiral(p:TSpiral):TSpiral;
    function degree(p1,p2,p3:TSpiral):extended;
    procedure deleteSpiral(var s: TSpiral);
    function distance(p1,p2:TSpiral):extended;
    procedure drawAllChildsUnlessFirstInSpiral(p:TSpiral;
                            penStyles:array of TPenStyle; penColors:array of TColor);
    procedure drawfirstChildOfFirstInSpiral(p:TSpiral; penStyle: TPenStyle; penColor: TColor);
    procedure drawMainSpiral(p:TSpiral; style:TPenStyle;
                                          color:TColor; Size:integer; numbersColor:TColor);
    procedure drawRoundRectangle(size: integer; color:TColor);
    procedure drawSpiral(p:TSpiral);
    procedure drawSpiralWithColors(p:TSpiral; mainColor,numbersColor:TColor;
                                          penColors:array of TColor);
    function equalCoordinates(p1,p2:TSpiral): Boolean;
    function findMatchPointForMaxDegreeToEnd(p1,p2,p:TSpiral):TSpiral;
    procedure formCreate(Sender: TObject);
    procedure formResize(Sender: TObject);
    procedure initializeRandomSpiral(var p:TSpiral; maxSpeedsArray:array of integer);
    procedure linkChilds(p:TSpiral);
    procedure pbPaint(Sender: TObject);
    procedure placeLog(s1, s2: TSpiral; var resetWithDoublePoints: Boolean);
    procedure reNumber(p:TSpiral);
    procedure timer1Timer(Sender: TObject);
    procedure validateCoordinates(p:TSpiral);
    procedure FormShow(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    { Private declarations }
    s: TSpiral;
    exitFlag: Boolean;
    log: TLogRecord;
    logQueue: TOverheadQueue;
    form2ShowFlag: Boolean;
    procedure backupSpiral(s: TSpiral; var copyOfS: TSpiral);
    procedure refreshPaintBoxSize;
  public
    { Public declarations }
    pointsNumber: Integer;
    pbWidth: Integer;
    pbHeight: Integer;
    formWidth: Integer;
    formHeight: Integer;
    maxPaintBoxWidth: Integer;
    maxPaintBoxHeight: Integer;
    maxSpeed: Integer;
    convergencyQueueNumber: Integer;
    convergencyAccuracy: Real;
    procedure applySizes;
    procedure setDefaultSizes;
    function calculateConvergencyAccuracy(number: Integer): Real;
  end;

var
  Form1: TForm1;
  function calculateRegionDegree(x,y:Extended):RegionDegree;
  function calculateDegree(rd1,rd2:RegionDegree):Extended;
implementation

uses Math, DateUtils, Unit2;
var
  drawFlag:Boolean;
///////////////////////////////////////////////////////////////////////////////
  procedure TForm1.linkChilds(p:TSpiral);
  var
    q:TSpiral;
    deg:Extended;
  begin
    if p<>nil then
    begin
      q:= p^.next;
      if q=nil then
      begin
        p^.FC:= nil;
        p^.LC:= nil;
      end
      else
      begin
        if q^.next=nil then
        begin
          p^.FC:= nil;
          p^.LC:= nil;
        end
        else
        begin
          while q^.next<>nil do
          begin
            deg:= degree(q, q^.next, p);
            if deg<0 then //q is firstChild
              Break;
            q:= q^.next;
          end;
          p^.FC:= q;
          while q^.next<>nil do
          begin
            deg:= degree(q, q^.next, p);
            if deg>0 then
              break;
            q:=q^.next;
          end;
          p^.LC:= q;
        end;
      end;
    end;
  end;
///////////////////////////////////////////////////////////////////////////////
  function calculateRegionDegree(x,y:Extended):RegionDegree;
  var
    rd:RegionDegree;
  begin
      if (x>=0) and (y>=0) then
        rd.region:= 1;
      if (x<0) and (y>0) then
        rd.region:= 2;
      if (x<=0) and (y<=0) then
        rd.region:= 3;
      if (x>0) and (y<0) then
        rd.region:= 4;
      if x=0 then
        rd.degreeWithXCoords:=90
      else
        rd.degreeWithXCoords:= RadToDeg(arctan(abs(y/x)));
      calculateRegionDegree:= rd;
  end;
///////////////////////////////////////////////////////////////////////////////
  function calculateDegree(rd1,rd2:RegionDegree):Extended;
  var
    d1Param,d2Param,ifCond,param180: integer;
    degree:Extended;
  begin
      d1Param:= 1 - 2*(rd1.region mod 2);
      d2Param:= 2*(rd2.region mod 2) - 1;
      if abs(rd1.region - rd2.region)=2 then
        if ((rd1.region=3) and (rd2.region=1)) then
          ifCond:=-1
        else
          ifCond:=-1
      else
        ifCond:=0;

      param180:=0;
      if (
          ((rd1.region=1) and (rd2.region=2)) or
          ((rd1.region=1) and (rd2.region=3)) or
          ((rd1.region=2) and (rd2.region=4)) or
          ((rd1.region=3) and (rd2.region=1)) or
          ((rd1.region=3) and (rd2.region=4)) or
          ((rd1.region=4) and (rd2.region=2))
         ) then
        param180:= 1;
      if (
          ((rd1.region=2) and (rd2.region=1)) or
          ((rd1.region=4) and (rd2.region=3))
         ) then
        param180:=-1;

      degree:= d1Param*rd1.degreeWithXCoords + d2Param*rd2.degreeWithXCoords + param180*180;
      if (abs(ifCond)=1) and (degree>180) then
        degree:= (360-degree)*ifCond;
      calculateDegree:= degree;
  end;
{$R *.dfm}
///////////////////////////////////////////////////////////////////////////////
function Tform1.distance(p1,p2:TSpiral):extended;
begin
    distance:=sqrt((p1^.x -p2^.x) * (p1^.x -p2^.x) + (p1^.y -p2^.y) * (p1^.y -p2^.y));
end;
///////////////////////////////////////////////////////////////////////////////
function TForm1.degree(p1,p2,p3:TSpiral):extended;
var
  x1,y1,x3,y3:extended;
  rd1,rd3: RegionDegree;
begin
  if (
      (equalCoordinates(p1,p2)) or
      (equalCoordinates(p2,p3)) or
      (equalCoordinates(p1,p3))
      ) then
    degree:=0
  else
  begin
    x1:= p1^.x - p2^.x;
    y1:= p1^.y - p2^.y;
    x3:= p3^.x - p2^.x;
    y3:= p3^.y - p2^.y;
    rd1:= calculateRegionDegree(x1,y1);
    rd3:= calculateRegionDegree(x3,y3);
    degree:= calculateDegree(rd1,rd3);
  end;
end;
///////////////////////////////////////////////////////////////////////////////
function TForm1.equalCoordinates(p1,p2:TSpiral): Boolean;
begin
  if ((p1^.x = p2^.x) and (p1^.y = p2^.y)) then
    equalCoordinates:= true
  else
    equalCoordinates:= false;
end;
///////////////////////////////////////////////////////////////////////////////
function TForm1.findMatchPointForMaxDegreeToEnd(p1,p2,p:TSpiral):TSpiral;
var
  answer: TSpiral;
  answerDegree, thisDegree: extended;
begin
  answer:=nil;
  answerDegree:= -400; ////////////////
  while p <> nil do
  begin
    if equalCoordinates(p2,p) then
    begin
      answer:= p;
      break;
    end;
    thisDegree:= degree(p1,p2,p);
    if (
       (thisDegree>answerDegree) or
       ((thisDegree = answerDegree) and (distance(p2, p)< distance(p2,answer)))
       ) then
    begin
      answerDegree:= thisDegree;
      answer:=p;
    end;
    p:= p^.Next;
  end;
  findMatchPointForMaxDegreeToEnd:= answer;
end;
///////////////////////////////////////////////////////////////////////////////
procedure TForm1.initializeRandomSpiral(var p:TSpiral; maxSpeedsArray:array of integer);
var
  i:integer;
  s1,temp:TSpiral;
begin
  while p<>nil do
  begin//removing current p
    s1:=p;
    p:= p^.next;
    dispose(s1);
  end;
  new(p);
  //randomize;
  p^.x:= 1+random(pb.Width-2);
  p^.y:= TEXT_HEIGHT+1+random(pb.Height-TEXT_HEIGHT-2);
  p^.id:= 1;
  p^.dx:= (1-2*random(2))*random(maxSpeedsArray[0]); //points pes 100 secound
  p^.dy:= (1-2*random(2))*random(maxSpeedsArray[0]);
  p^.prior:= nil;
  p^.FC:= nil;
  p^.LC:= nil;
  temp:= p;//start

  for i:=2 to pointsNumber do
  begin
    new(s1);
    s1^.x:= 1+random(pb.Width-2);
    s1^.y:= TEXT_HEIGHT+1+random(pb.Height-TEXT_HEIGHT-2);
    s1^.Id:=i;
    s1^.dx:= (1-2*random(2))*random(maxSpeedsArray[i-1]);
    s1^.dy:= (1-2*random(2))*random(maxSpeedsArray[i-1]);
    s1^.prior:= p;
    p^.next:=s1;
    s1^.FC:= nil;
    s1^.LC:= nil;
    p:=s1;
  end;
  p^.next:=nil;
  p:= convertToCorrectSpiral(temp);
  validateCoordinates(p);
  reNumber(p);
end;
///////////////////////////////////////////////////////////////////////////////
function TForm1.convertToCorrectSpiral(p:TSpiral):TSpiral;
var
  leftmostX:extended;
  s1,s2,s3,temp,temp1:TSpiral;
begin
  //finding leftmost point and linking...:
  temp:=p;
  s2:=p;
  leftmostX:= s2^.x;
  p:= p^.next;
  while p<>nil do
  begin
    if ((p^.x < leftmostX) or ((p^.x = leftmostX) and (p^.y < s2^.y))) then
    begin
      leftmostX:= p^.x;
      s2:= p;
    end;
    p:= p^.next;
  end;
  if s2<>temp then//else; if s2 = start of spiral ==> does not needs changes.
  begin
    p:=s2;
    p^.prior^.next:= p^.next;
    if p^.next<>nil then
      p^.next^.prior:= p^.prior;
    p^.next:= temp;
    temp^.prior:= p;
    p^.prior:=nil;
    temp:=p;
  end;
  //finding next point from assumptive point and first point
  new(s1);
  s1^.x:= s2^.x;
  s1^.y:= s2^.y-1;
  temp1:=s1;
  while s2^.next<>nil do
  begin
    s3:= findMatchPointForMaxDegreeToEnd(s1, s2, s2^.next);
    if s3 <> s2^.next then
    begin
      p:=s3;
      p^.prior^.next:= p^.next;
      if s3^.next <> nil then
        p^.next^.prior:= p^.prior;
      p^.next:= s2^.next;
      s2^.next^.prior:= p;
      s2^.next:= p;
      p^.prior:= s2;
    end;
    s1:= s2;
    s2:= s3;
  end;
  Dispose(temp1);
  p:=temp;
  //renumbering based on spiral rounds:
  p^.prior:= nil;
  while p<>nil do
  begin
    p^.FC:= nil;
    p^.LC:= nil;
    linkChilds(p);
    p:=p^.next;
  end;
  convertToCorrectSpiral:= temp;
end;
///////////////////////////////////////////////////////////////////////////////
procedure TForm1.reNumber(p:TSpiral);
var
  i:integer;
begin
  i:=1;
  while p<>nil do
  begin
    p^.id:= i;
    p:= p^.next;
    i:= i+1;
  end;
end;
///////////////////////////////////////////////////////////////////////////////
procedure TForm1.drawSpiral(p:TSpiral);
var
  childsPenColors: array[0..14] of TColor;
begin
  childsPenColors[1]:= $000000ff;
  childsPenColors[2]:= $00ff0000;
  childsPenColors[3]:= $0000ff00;
  childsPenColors[4]:= $00000077;
  childsPenColors[5]:= $00770000;

  childsPenColors[6]:= $0000ffff;
  childsPenColors[7]:= $00ffff00;
  childsPenColors[8]:= $00ff00ff;

  childsPenColors[9]:= $00007777;
  childsPenColors[10]:= $00777700;
  childsPenColors[11]:= $00770077;

  childsPenColors[12]:= $00777777;
  childsPenColors[13]:= clActiveCaption;
  childsPenColors[14]:= clInfoBk;

  childsPenColors[0]:= $00007700;

  drawSpiralWithColors(p, clBlack, clPurple, childsPenColors);
end;
///////////////////////////////////////////////////////////////////////////////
procedure TForm1.clearSpiral(p:TSpiral);
var
  i:integer;
  childsPenColors: array[0..MAX_POINTS_NUMBER] of TColor;
begin
  for i:=0 to pointsNumber-1 do
    childsPenColors[i]:= clWhite;
  drawSpiralWithColors(p, clWhite, clWhite, childsPenColors);
end;
///////////////////////////////////////////////////////////////////////////////
procedure TForm1.drawSpiralWithColors(p:TSpiral; mainColor,numbersColor:TColor;
                                      penColors:array of TColor);
var
  i:integer;
  childsPenStyles:array[0..MAX_POINTS_NUMBER] of TPenStyle;
begin
  if p<>nil then
  begin
    for i:=0 to pointsNumber-1 do
      case (i mod 3) of
        0:childsPenStyles[i]:= psDot;
        1:childsPenStyles[i]:= psDashDot;
        2:childsPenStyles[i]:= psDashDotDot;
      end;
    drawMainSpiral(p, psSolid, mainColor, 3, numbersColor);//mainColor=clBlack, numbersColor=clPurple
    drawAllChildsUnlessFirstInSpiral(p, childsPenStyles, penColors);
    drawfirstChildOfFirstInSpiral(p, childsPenStyles[p^.Id], penColors[1]);
    drawRoundRectangle(1,clGray);
  end;
end;
///////////////////////////////////////////////////////////////////////////////
procedure TForm1.drawMainSpiral(p:TSpiral; style:TPenStyle;
                                color:TColor; Size:integer;
                                numbersColor:TColor);
var
  str1:string;
  temp:TSpiral;
begin
  temp:=p;
  pb.Canvas.MoveTo(round(p^.x),round(p^.y));

  pb.canvas.Font.Style:=[fsItalic, fsBold];
  pb.Canvas.Font.Size:= pb.Canvas.Font.Size+4;
  pb.Canvas.Font.Color:= numbersColor; //clPurple;
  while p<>nil do //writing the numbers:
  begin
    str(p^.Id:0,str1);
    pb.Canvas.TextOut(round(p^.x),round(p^.y),str1);
    pb.Canvas.MoveTo(round(p^.x),round(p^.y));
    p:=p^.next;
  end;
  p:=temp;
  pb.Canvas.Font.Size:= pb.Canvas.Font.Size-4;
  pb.canvas.Font.Style:=[];

  pb.Canvas.Pen.Color:= color;
  pb.Canvas.Pen.Width:= size;
  pb.Canvas.pen.Style:= style;
  pb.Canvas.MoveTo(round(p^.x),round(p^.y));
  p:=p^.next;
  while p<>nil do //drawing the lines:
  begin
    pb.Canvas.LineTo(round(p^.x),round(p^.y));
    p:=p^.next;
  end;
end;
///////////////////////////////////////////////////////////////////////////////
procedure TForm1.drawAllChildsUnlessFirstInSpiral(
                                    p:TSpiral;
                                    penStyles:array of TPenStyle;
                                    penColors:array of TColor);
begin
    pb.Canvas.Pen.Width:= 1;
    while p^.next<>nil do
    begin
      pb.Canvas.pen.Style:= penStyles[p^.Id mod pointsNumber];
      pb.Canvas.Pen.Color:= penColors[p^.Id mod pointsNumber];
      if p^.prior<>nil then
      begin
        pb.Canvas.MoveTo(round(p^.x), round(p^.y));
        if p^.FC<>nil then
          pb.Canvas.LineTo(round(p^.FC^.x), round(p^.FC^.y));
      end;
      pb.Canvas.MoveTo(round(p^.x), round(p^.y));
      if p^.LC<>nil then
        pb.Canvas.LineTo(round(p^.LC^.x), round(p^.LC^.y));
      p:= p^.next;
    end;
end;
///////////////////////////////////////////////////////////////////////////////
procedure TForm1.drawfirstChildOfFirstInSpiral(p:TSpiral; penStyle: TPenStyle; penColor: TColor);
begin
  if p<>nil then
    if p^.FC<>nil then
    begin
      pb.Canvas.Pen.Width:= 2;
      pb.Canvas.pen.Style:= penStyle;
      pb.Canvas.Pen.Color:= penColor;
      pb.Canvas.MoveTo(round(p^.x), round(p^.y));
      pb.Canvas.LineTo(round(p^.FC^.x), round(p^.FC^.y));
    end;
end;
///////////////////////////////////////////////////////////////////////////////
procedure TForm1.drawRoundRectangle(size: integer; color:TColor);
begin
  pb.Canvas.Pen.Width:= size;
  pb.Canvas.Pen.Color:= color;
  pb.Canvas.Pen.Style:= psSolid;

  pb.Canvas.MoveTo(0, TEXT_HEIGHT-1);
  pb.Canvas.LineTo(pb.width, TEXT_HEIGHT-1);
end;
///////////////////////////////////////////////////////////////////////////////
procedure TForm1.FormResize(Sender: TObject);
begin
  if self.Width < formWidth then
    self.Width:= formWidth;
  if self.height < formHeight then
    self.height:= formHeight;
  drawFlag:= false;
  validateCoordinates(s);
  if s <> nil then
    s:= convertToCorrectSpiral(s);
  drawFlag:= true;
  refreshPaintBoxSize;
end;
///////////////////////////////////////////////////////////////////////////////
procedure TForm1.pbPaint(Sender: TObject);
begin
  if pointsNumber <= MAX_POINTS_NUMBER_TO_DRAW_SPIRAL then
    begin
    pb.Canvas.Brush.Color := clWhite;
    pb.Canvas.FillRect(pb.Canvas.ClipRect);
    drawRoundRectangle(1, clGray);
    if pointsNumber <= MAX_POINTS_NUMBER_TO_DRAW_SPIRAL then
      drawSpiral(s);
    end;
end;
///////////////////////////////////////////////////////////////////////////////
procedure TForm1.calculateNewCoordinates(p:TSpiral);
  var
    temp: TSpiral;
begin
  temp:= p;
  while p<>nil do
  begin
    p^.x:= p^.x + p^.dx*timer1.Interval/100000000;
    p^.y:= p^.y + p^.dy*timer1.Interval/100000000;
    p:= p^.next;
  end;
  p:= temp;
  validateCoordinates(p);
end;
///////////////////////////////////////////////////////////////////////////////
procedure TForm1.FormCreate(Sender: TObject);
begin
  exitFlag:= true;
  drawFlag:= false;
  form2ShowFlag:= true;
  setDefaultSizes;
end;
///////////////////////////////////////////////////////////////////////////////
procedure TForm1.Timer1Timer(Sender: TObject);
var
  copyOfS: TSpiral;
  resetWithDoublePoints: Boolean;
begin
  if s <> nil then
  begin
    if (drawFlag) then
    begin
      drawFlag:= false;
      exitFlag:= false;
      backupSpiral(s, copyOfS);

      calculateNewCoordinates(s);
      s:= convertToCorrectSpiral(s);
      if pointsNumber <= MAX_POINTS_NUMBER_TO_DRAW_SPIRAL then
        begin
        clearSpiral(copyOfS);
        drawSpiral(s);
        end;

      placeLog(copyOfS, s, resetWithDoublePoints);
      deleteSpiral(copyOfS);

      exitFlag:= true;
      drawFlag:= true;
      if resetWithDoublePoints then
        begin
        form2.edit1.Text:= intToStr(2*strToInt(form2.edit1.Text));
        form2.Button1Click(self);
        end;
    end;
  end;
end;
///////////////////////////////////////////////////////////////////////////////
procedure TForm1.validateCoordinates(p:TSpiral);
begin
  while p<> nil do
    begin
    if p^.x>=pb.Width-1.5 then
      begin
      p^.x:= pb.Width - p^.x + pb.Width -3;
      p^.dx:= -p^.dx;
      end
    else
      if p^.x<=0.5 then
        begin
        p^.x:= 1-p^.x;
        p^.dx:= -p^.dx;
        end;

    if p^.y>=pb.Height-1.5 then
      begin
      p^.y:= 2*pb.Height-3-p^.y;
      p^.dy:= -p^.dy;
      end
    else
      if p^.y<=TEXT_HEIGHT-0.5 then
        begin
        p^.y:= 2*TEXT_HEIGHT-1-p^.y;
        p^.dy:= -p^.dy;
        end;
        
    p:= p^.next;
    end; //while.
end;
///////////////////////////////////////////////////////////////////////////////
procedure TForm1.backupSpiral(s: TSpiral; var copyOfS: TSpiral);
  var
    s1, temp: TSpiral;
    i: Integer;
begin
  //copying to copyOfS:
  new(copyOfS);
  copyOfS^.x:= s^.x;
  copyOfS^.y:= s^.y;
  copyOfS^.id:= s^.id;
  copyOfS^.dx:= s^.dx;
  copyOfS^.dy:= s^.dy;
  copyOfS^.prior:= nil;
  copyOfS^.FC:= nil;
  copyOfS^.LC:= nil;
  temp:= copyOfS;

  for i:=2 to pointsNumber do
  begin
    new(s1);
    s:= s^.next;
    s1^.x:= s^.x;
    s1^.y:= s^.y;
    s1^.Id:= s^.Id;
    s1^.dx:= s^.dx;
    s1^.dy:= s^.dy;
    s1^.prior:= copyOfS;
    s1^.next:= nil;
    copyOfS^.next:= s1;
    s1^.FC:= nil;
    s1^.LC:= nil;
    copyOfS:= s1;
  end;
  copyOfS:= temp;
  copyOfS:= convertToCorrectSpiral(copyOfS);
end;
///////////////////////////////////////////////////////////////////////////////
procedure TForm1.deleteSpiral(var s: TSpiral);
  var
    s1: TSpiral;
begin
  while s <> nil do
  begin
    s1:= s;
    s:= s^.next;
    dispose(s1);
  end;
end;
///////////////////////////////////////////////////////////////////////////////
procedure TForm1.placeLog(s1, s2: TSpiral; var resetWithDoublePoints: Boolean);
  Type
    TAllPointsPointers= array[0..MAX_POINTS_NUMBER] of TSpiral;
  var
    i, changedP, firstC1, lastC1, firstC2, lastC2, allChangedPointsCounter, pointsOnCH: Integer;
    GoInOrGoOutOccurred, otherEventOccurred, externalEventOccurred, internalEventOccured: Boolean;
    a1, a2: TAllPointsPointers;
    eventType: TEventType;
    overheadIE, overheadEI, AllToConvexHullRatio, tempStr, logText1, logText2, logText3, fileName: String;
    ff: TextFile;
    j: TEventType;
    temp1, temp2: TSpiral;
begin
  resetWithDoublePoints:= false;
  temp1:= s1;
  temp2:= s2;
  eventType:= NOTHING;
  changedP:= 0;
  GoInOrGoOutOccurred:= false;
  otherEventOccurred:= false;
  if (s1^.id <> s2^.id) and (s1^.next^.id = s2^.next^.id) then
    begin
    eventType:= GI;
    GoInOrGoOutOccurred:= true;
    a1[s1^.id]:= s1;
    a2[s2^.id]:= s2;
    s1:= s1^.next;
    s2:= s2^.next;
    end; //if
  for i:= 1 to pointsNumber do
  begin
    a1[s1^.id]:= s1;
    a2[s2^.id]:= s2;
    if (s1^.id <> s2^.id) and (not GoInOrGoOutOccurred) then
      begin
      GoInOrGoOutOccurred:= true;
      if (s1^.next <> nil) and (s1^.next^.id = s2^.id) then
        eventType:= GI
      else
        if (s2^.next <> nil) and (s1^.id = s2^.next^.id) then
          eventType:= GO
      end; //if
    s1:= s1^.next;
    s2:= s2^.next;
  end; //while

  for i:= 1 to pointsNumber do
  begin
    if a1[i]^.FC = nil then
      firstC1:= 0
    else
      firstC1:= a1[i]^.FC^.id;
    if a2[i].FC = nil then
      firstC2:= 0
    else
      firstC2:= a2[i]^.FC^.id;

    if a1[i]^.LC = nil then
      lastC1:= 0
    else
      lastC1:= a1[i]^.LC^.id;
    if a2[i].LC = nil then
      lastC2:= 0
    else
      lastC2:= a2[i]^.LC^.id;

    if (firstC1 <> firstC2) or (lastC1 <> lastC2) then
    begin
      if (eventType = CD) then
        eventType:= RESIZE_PAINTBOX
      else
        if not GoInOrGoOutOccurred then
          begin
          otherEventOccurred:= true;
          eventType:= BE_FC;
          end;
      inc(changedP);
    end;

    if (a1[i]^.dx <> a2[i]^.dx) or (a1[i]^.dy <> a2[i]^.dy) then
    begin
      if (GoInOrGoOutOccurred) or (eventType = BE_FC) then
        eventType:= RESIZE_PAINTBOX
      else
        begin
        otherEventOccurred:= true;
        eventType:= CD;
        end;
      inc(changedP);
    end;  

  end;

  inc(log.totalAttemptNumber);
  if ((GoInOrGoOutOccurred) or (otherEventOccurred)) then
  begin
    if (GoInOrGoOutOccurred) and ((changedP = 0) or (eventType = NOTHING)) then
      eventType:= RESIZE_PAINTBOX;
    if (eventType <> RESIZE_PAINTBOX) then
      begin
      if eventType <> CD then
        inc(log.totalOccurredEvents);
      if (otherEventOccurred) and (changedP>1) then
        begin
        if eventType <> CD then
          inc(log.totalOccurredEvents, changedP-1);
        inc(log.occurredEvent[eventType], changedP-1);
        end; //if (othe...
      end; //if (even...
    inc(log.occurredEvent[eventType]);
    inc(log.changedPointsAfterOccurrence[eventType], changedP);
  end; //if ((GoI...

  //Determining the internal/external status:
  s1:= temp1;
  s2:= temp2;
  if ((GoInOrGoOutOccurred) or (otherEventOccurred)) and (eventType <> CD) then
    begin
    internalEventOccured:= true;

    if temp1^.FC^.id <> temp2^.FC^.id then
      internalEventOccured:= false
    else
      while (s1 <> nil) and (s2 <> nil) do
        begin
        if s1^.id <> s2^.id then
          begin
          internalEventOccured:= false;
          break;
          end;
        if temp1^.FC^.id = s1^.id then
          break;
        s1:= s1^.next;
        s2:= s2^.next;
        end; //while.

    if internalEventOccured then
      begin
      if (otherEventOccurred) and (changedP>1) and (eventType <> RESIZE_PAINTBOX) then
        inc(log.occurredEvent[INTERNAL_EV], changedP-1);
      inc(log.occurredEvent[INTERNAL_EV]);
      inc(log.changedPointsAfterOccurrence[INTERNAL_EV], changedP);
      if eventType = RESIZE_PAINTBOX then
        inc(log.totalOccurredEvents); //In this case it was not added before.
      end
    else
      begin
      if (otherEventOccurred) and (changedP>1) and (eventType <> RESIZE_PAINTBOX) then
        inc(log.occurredEvent[EXTERNAL_EV], changedP-1);
      inc(log.occurredEvent[EXTERNAL_EV]);
      inc(log.changedPointsAfterOccurrence[EXTERNAL_EV], changedP);
      if eventType = RESIZE_PAINTBOX then
        inc(log.totalOccurredEvents); //In this case it was not added before.
      end; //else.
    logQueue.enqueue(log.occurredEvent[EXTERNAL_EV], log.occurredEvent[INTERNAL_EV], log.changedPointsAfterOccurrence[EXTERNAL_EV], log.changedPointsAfterOccurrence[INTERNAL_EV]);
    if (log.totalOccurredEvents >= MINIMUM_EVENTS_TO_OCCUR_BEFORE_RESET) and (logQueue.isValidWithAccuracy()) then
      resetWithDoublePoints:= true;
    end; //if ((GoI...

  //Calculating the "CH points / All Points" ratio:
  pointsOnCH:= 0;
  s1:= temp1;
  while s1 <> nil do
    begin
    inc(pointsOnCH);
    if temp1^.FC^.id = s1^.id then
      break;
    s1:= s1^.next;
    end;
  str((pointsNumber/pointsOnCH):0:2, AllToConvexHullRatio);

  if CONVERGENCY_PARAMETER_IS_OVERHEAD then
    begin
    if log.changedPointsAfterOccurrence[INTERNAL_EV] = 0 then
      overheadEI:= '?????'
    else
      str(((log.changedPointsAfterOccurrence[EXTERNAL_EV])/log.changedPointsAfterOccurrence[INTERNAL_EV]):0:3, overheadEI);
    if (log.changedPointsAfterOccurrence[EXTERNAL_EV]) = 0 then
      overheadIE:= '?????'
    else
      str((log.changedPointsAfterOccurrence[INTERNAL_EV]/(log.changedPointsAfterOccurrence[EXTERNAL_EV])):0:3, overheadIE);
    end //if CONVE....
  else
    begin
    if log.occurredEvent[INTERNAL_EV] = 0 then
      overheadEI:= '?????'
    else
      str(((log.occurredEvent[EXTERNAL_EV])/log.occurredEvent[INTERNAL_EV]):0:4, overheadEI);
    if (log.occurredEvent[EXTERNAL_EV]) = 0 then
      overheadIE:= '?????'
    else
      str((log.occurredEvent[INTERNAL_EV]/(log.occurredEvent[EXTERNAL_EV])):0:4, overheadIE);
    end; //else of if CONVE....

  if CONVERGENCY_PARAMETER_IS_OVERHEAD then
    tempStr:= ' overhead'
  else
    tempStr:= '';
  logText1:= 'N: ' + intToStr(pointsNumber) +  '       #: ' + intToStr(log.totalAttemptNumber)
    + '      Int/Ext' + tempStr + ' : ' + overheadIE
    + '      Ext/Int' + tempStr + ': ' + overheadEI + '      CH: ' + intToStr(pointsOnCH) + ' ==> ALL/CH: ' + AllToConvexHullRatio + '          ';

  allChangedPointsCounter:= 0;
  for j:= GI to NOT_LC do
    allChangedPointsCounter:= allChangedPointsCounter + log.changedPointsAfterOccurrence[j];
  allChangedPointsCounter:= allChangedPointsCounter + log.changedPointsAfterOccurrence[RESIZE_PAINTBOX];

  logText2:= 'Total:    ' + intToStr(log.totalOccurredEvents) + ' ==> ' + intToStr(allChangedPointsCounter) + ' changed.'
    + '             External: ' + intToStr(log.occurredEvent[EXTERNAL_EV]) + ' ==> ' + intToStr(log.changedPointsAfterOccurrence[EXTERNAL_EV]) + ' changed.'
    + '                   Internal: ' + intToStr(log.occurredEvent[INTERNAL_EV]) + ' ==> ' + intToStr(log.changedPointsAfterOccurrence[INTERNAL_EV]) + ' changed.' + '          ';

  logText3:= 'GI: ' + intToStr(log.occurredEvent[GI]) + ' ==> ' + intToStr(log.changedPointsAfterOccurrence[GI]) + ' changed.'
    + '         GO: ' + intToStr(log.occurredEvent[GO]) + ' ==> ' + intToStr(log.changedPointsAfterOccurrence[GO]) + ' changed.'
    + '         Child: ' + intToStr(log.occurredEvent[BE_FC]) + ' ==> ' + intToStr(log.changedPointsAfterOccurrence[BE_FC]) + ' changed.'
    + '         CD: ' + intToStr(log.occurredEvent[CD]) + ' ==> ' + intToStr(log.changedPointsAfterOccurrence[CD]) + ' changed.'
    + '         Resize Box:       ' + intToStr(log.occurredEvent[RESIZE_PAINTBOX]) + ' ==> ' + intToStr(log.changedPointsAfterOccurrence[RESIZE_PAINTBOX]) + ' changed.' + '          ';
  pb.Canvas.TextOut(0, 0 ,logText1);
  pb.Canvas.TextOut(0, 12 ,logText2);
  pb.Canvas.TextOut(0, 24 ,logText3);
  fileName:= 'Results' + intToStr(pointsNumber) + '.txt';
  AssignFile(ff, fileName);
  if (log.totalAttemptNumber = 1) or (GoInOrGoOutOccurred) or (otherEventOccurred) then
    begin
    if FileExists(fileName) then
      append(ff)
    else
      rewrite(ff);
    if log.totalAttemptNumber = 1 then
      begin
      writeln(ff, '||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||');
      writeln(ff, '|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||| '+intToStr(pointsNumber)+' Poins: ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||');
      write(ff, 'N');
      write(ff, chr(9));
      write(ff, 'Att#');
      write(ff, chr(9));
      write(ff, 'Tot');
      write(ff, chr(9));
      write(ff, 'Ch Tot');
      write(ff, chr(9));
      write(ff, 'Ext');
      write(ff, chr(9));
      write(ff, 'Ch Ext');
      write(ff, chr(9));
      write(ff, 'Int');
      write(ff, chr(9));
      write(ff, 'Ch Int'); write(ff, chr(9));
      write(ff, 'GI'); write(ff, chr(9));
      write(ff, 'Ch GI'); write(ff, chr(9));
      write(ff, 'GO'); write(ff, chr(9));
      write(ff, 'Ch GO'); write(ff, chr(9));
      write(ff, 'Chld'); write(ff, chr(9));
      write(ff, 'Ch Chld'); write(ff, chr(9));
      write(ff, 'CD'); write(ff, chr(9));
      write(ff, 'Ch CD'); write(ff, chr(9));
      write(ff, 'BOX'); write(ff, chr(9));
      write(ff, 'Ch BOX'); write(ff, chr(9));
      write(ff, 'CH'); write(ff, chr(9));
      write (ff, 'All/CH'); write(ff, chr(9));
      write(ff, 'OverhIE'); write(ff, chr(9));
      writeln(ff, 'overhEI');
      end;
    if ((GoInOrGoOutOccurred) or (otherEventOccurred)) then
      begin
      write(ff, intToStr(pointsNumber));
      write(ff, chr(9));
      write(ff, intToStr(log.totalAttemptNumber));
      write(ff, chr(9));
      write(ff, intToStr(log.totalOccurredEvents));
      write(ff, chr(9));
      write(ff, intToStr(allChangedPointsCounter));
      write(ff, chr(9));
      write(ff, intToStr(log.occurredEvent[EXTERNAL_EV]));
      write(ff, chr(9));
      write(ff, intToStr(log.changedPointsAfterOccurrence[EXTERNAL_EV]));
      write(ff, chr(9));
      write(ff, intToStr(log.occurredEvent[INTERNAL_EV]));
      write(ff, chr(9));
      write(ff, intToStr(log.changedPointsAfterOccurrence[INTERNAL_EV]));
      write(ff, chr(9));
      write(ff, intToStr(log.occurredEvent[GI]));
      write(ff, chr(9));
      write(ff, intToStr(log.changedPointsAfterOccurrence[GI]));
      write(ff, chr(9));
      write(ff, intToStr(log.occurredEvent[GO]));
      write(ff, chr(9));
      write(ff, intToStr(log.changedPointsAfterOccurrence[GO]));
      write(ff, chr(9));
      write(ff, intToStr(log.occurredEvent[BE_FC]));
      write(ff, chr(9));
      write(ff, intToStr(log.changedPointsAfterOccurrence[BE_FC]));
      write(ff, chr(9));
      write(ff, intToStr(log.occurredEvent[CD]));
      write(ff, chr(9));
      write(ff, intToStr(log.changedPointsAfterOccurrence[CD]));
      write(ff, chr(9));
      write(ff, intToStr(log.occurredEvent[RESIZE_PAINTBOX]));
      write(ff, chr(9));
      write(ff, intToStr(log.changedPointsAfterOccurrence[RESIZE_PAINTBOX]));
      write(ff, chr(9));
      write(ff, intToStr(pointsOnCH));
      write(ff, chr(9));
      write(ff, allToConvexHullRatio);
      write(ff, chr(9));
      write(ff, overheadIE);
      write(ff, chr(9));
      writeln(ff, overheadEI);
      end;
    closeFile(ff);
    end;
{
  AssignFile(ff, 'allResults.txt');
  append(ff);
  if log.totalAttemptNumber = 1 then
    begin
    writeln(ff, '||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||');
    writeln(ff, '|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||| '+intToStr(pointsNumber)+' Poins: ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||');
    write(ff, 'N');
    write(ff, chr(9));
    write(ff, 'Attempt#');
    write(ff, chr(9));
    write(ff, 'Total');
    write(ff, chr(9));
    write(ff, 'Changed');
    write(ff, chr(9));
    write(ff, 'External');
    write(ff, chr(9));
    write(ff, 'Changed E');
    write(ff, chr(9));
    write(ff, 'Internal');
    write(ff, chr(9));
    write(ff, 'Changed I'); write(ff, chr(9));
    write(ff, 'GI'); write(ff, chr(9));
    write(ff, 'Changed GI'); write(ff, chr(9));
    write(ff, 'GO'); write(ff, chr(9));
    write(ff, 'Changed GO'); write(ff, chr(9));
    write(ff, 'Others'); write(ff, chr(9));
    write(ff, 'Changed Others'); write(ff, chr(9));
    write(ff, 'RESIZE_BOX'); write(ff, chr(9));
    write(ff, 'Changed RESIZE_BOX'); write(ff, chr(9));
    write(ff, 'OverheadIE'); write(ff, chr(9));
    writeln(ff, 'overheadEI');
    end; //    writeln(ff, logText1);    writeln(ff, logText2);    writeln(ff, logText3);    writeln(ff, ' ');
  if (resetWithDoublePoints) or ((round(log.totalAttemptNumber*pointsNumber/4)) mod 100 = 0) then
    begin
    write(ff, intToStr(pointsNumber));
    write(ff, chr(9));
    write(ff, intToStr(log.totalAttemptNumber));
    write(ff, chr(9));
    write(ff, intToStr(log.totalOccurredEvents));
    write(ff, chr(9));
    write(ff, intToStr(allChangedPointsCounter));
    write(ff, chr(9));
    write(ff, intToStr(log.occurredEvent[EXTERNAL_EV]));
    write(ff, chr(9));
    write(ff, intToStr(log.changedPointsAfterOccurrence[EXTERNAL_EV]));
    write(ff, chr(9));
    write(ff, intToStr(log.occurredEvent[INTERNAL_EV]));
    write(ff, chr(9));
    write(ff, intToStr(log.changedPointsAfterOccurrence[INTERNAL_EV]));
    write(ff, chr(9));
    write(ff, intToStr(log.occurredEvent[GI]));
    write(ff, chr(9));
    write(ff, intToStr(log.changedPointsAfterOccurrence[GI]));
    write(ff, chr(9));
    write(ff, intToStr(log.occurredEvent[GO]));
    write(ff, chr(9));
    write(ff, intToStr(log.changedPointsAfterOccurrence[GO]));
    write(ff, chr(9));
    write(ff, intToStr(log.occurredEvent[CD]));
    write(ff, chr(9));
    write(ff, intToStr(log.changedPointsAfterOccurrence[CD]));
    write(ff, chr(9));
    write(ff, intToStr(log.occurredEvent[RESIZE_PAINTBOX]));
    write(ff, chr(9));
    write(ff, intToStr(log.changedPointsAfterOccurrence[RESIZE_PAINTBOX]));
    write(ff, chr(9));
    write(ff, overheadIE);
    write(ff, chr(9));
    writeln(ff, overheadEI);
    end
  else
    resetWithDoublePoints:= false;
  closeFile(ff);
}
end; //procedure TForm1.placeLog(....
////////////////////////////////////////////////////////////////////////////////
procedure TForm1.refreshPaintBoxSize;
begin
  if pointsNumber <= MAX_POINTS_NUMBER_TO_DRAW_SPIRAL then
    begin
    label1.Left:= pb.Left+pb.Width+1;
    label2.Top:= pb.Top+pb.Height+3;
    repaint;
    end;
end;
////////////////////////////////////////////////////////////////////////////////
procedure TForm1.FormShow(Sender: TObject);
begin
  if form2ShowFlag then
    form2.ShowModal;
end;
////////////////////////////////////////////////////////////////////////////////
procedure TForm1.applySizes;
  var
    i: TEventType;
begin
  self.Width:= formWidth;
  self.Height:= formHeight;
  pb.Width:= pbWidth;
  pb.Height:= pbHeight;

  maxSpeed:= round(700000/power((pointsNumber/4), 1.25));
  timer1.Interval:= round(40*sqrt(pointsNumber/4));

  log.totalAttemptNumber:= 0;
  log.totalOccurredEvents:= 0;
  for i:= GI to INTERNAL_EV do
    begin
    log.occurredEvent[i]:= 0;
    log.changedPointsAfterOccurrence[i]:= 0;
    end;
  if logQueue <> nil then
    logQueue.destroy();
  logQueue := TOverheadQueue.create();

  form2ShowFlag:= false;
  self.Position:= poScreenCenter;
  form2ShowFlag:= true;
  refreshPaintBoxSize;
end;
////////////////////////////////////////////////////////////////////////////////
procedure TForm1.setDefaultSizes;
begin
  pointsNumber:= DEFAULT_POINTS_NUMBER;
  convergencyQueueNumber:= 10;
  convergencyAccuracy:= calculateConvergencyAccuracy(pointsNumber);
  pbWidth:= DEFAULT_PB_WIDTH;
  pbHeight:= DEFAULT_PB_HEIGHT;
  formWidth:= DEFAULT_FORM_WIDTH;
  formHeight:= DEFAULT_FORM_HEIGHT;
  maxPaintBoxHeight:= DEFAULT_MAX_PAINTBOX_HEIGHT;
  maxPaintBoxWidth:= DEFAULT_MAX_PAINTBOX_WIDTH;

  applySizes;
end;
////////////////////////////////////////////////////////////////////////////////
function TForm1.calculateConvergencyAccuracy(number: Integer): Real;
begin
  if number < 100 then
    result:= 0.15
  else
    if number < 1000 then
      result:= 0.25
    else
      result:= 0.5;
end;
////////////////////////////////////////////////////////////////////////////////
procedure TForm1.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  maxSpeeds:array of integer;
  i:integer;
begin
  case key of
    ord('1'), 13:
    begin
      drawFlag:= false;
      exitFlag:= false;
      applySizes;
      pb.Invalidate;
      pb.Canvas.Brush.Color := clWhite;
      pb.Canvas.FillRect(pb.Canvas.ClipRect);
      setLength(maxSpeeds, pointsNumber);
      for i:=0 to pointsNumber-1 do
        maxSpeeds[i]:= maxSpeed;
      initializeRandomSpiral(s, maxSpeeds);
      if pointsNumber <= MAX_POINTS_NUMBER_TO_DRAW_SPIRAL then
        drawSpiral(s);
      drawFlag:= true;
    end; //'1'.
    ord('2'), 32:
    begin
      drawFlag:= not drawFlag;
    end; //'2'.
    112{F1}, ord('3'):
      form2.show;
    ord('0'), 27 :
    begin
      drawFlag:= false;
      while not exitFlag do
        ;
      deleteSpiral(s);
      ShowMessage('The results have appended to "allResults.txt"');
      Close;
    end; //'0'.
    39:
      begin
      if pb.width <> maxPaintBoxWidth then
      begin
        if pb.width+10 < maxPaintBoxWidth then
          pb.width:= pb.width+10
        else
          pb.width:= maxPaintBoxWidth;
        refreshPaintBoxSize;
      end;
      end;
    37:
      begin
      if pb.width <> formWidth-25 then
      begin
        if pb.Width-10 > formWidth-25 then
          pb.width:= pb.width-10
        else
          pb.width:= formWidth-25;
        refreshPaintBoxSize;
      end;
      end;
    38:
      begin
      if pb.Height <> maxPaintBoxHeight then
      begin
        if pb.Height+10 < maxPaintBoxHeight then
          pb.Height:= pb.Height+10
        else
          pb.Height:= maxPaintBoxHeight;
        refreshPaintBoxSize;
      end;
      end;
    40:
      if pb.Height <> formHeight-49 then
      begin
        if pb.Height-10 > formHeight-49 then
          pb.height:= pb.height-10
        else
          pb.height:= formHeight-49;
        refreshPaintBoxSize;
      end;
  end; //case key ....
end;
////////////////////////////////////////////////////////////////////////////////
constructor TOverheadQueue.create();
begin
  inherited create();
  first:= 0;
  last:= 0;
end;
////////////////////////////////////////////////////////////////////////////////
procedure TOverheadQueue.enqueue(item: TOverheadLogRecord);
begin
  last:= (last mod form1.convergencyQueueNumber) + 1;
  items[last]:= item;
  if (last = first) or (first = 0) then
    first:= (first mod form1.convergencyQueueNumber) + 1;
end;
////////////////////////////////////////////////////////////////////////////////
procedure TOverheadQueue.enqueue(externalOccurred, internalOccurred, externalChangedPointsAfterOccurrence, internalChangedPointsAfterOccurrence: Integer);
  var
    item: TOverheadLogRecord;
begin
  item.externalOccurred:= externalOccurred;
  item.internalOccurred:= internalOccurred;
  item.externalChangedPointsAfterOccurrence:= externalChangedPointsAfterOccurrence;
  item.internalChangedPointsAfterOccurrence:= internalChangedPointsAfterOccurrence;
  enqueue(item);
end;
////////////////////////////////////////////////////////////////////////////////
function TOverheadQueue.dequeue(): TOverheadLogRecord;
begin
  if (first = 0) then
    begin
    result.externalOccurred:= UNKNOWN;
    result.internalOccurred:= UNKNOWN;
    result.externalChangedPointsAfterOccurrence:= UNKNOWN;
    result.internalChangedPointsAfterOccurrence:= UNKNOWN;
    end //if.
  else
    begin
    result:= items[first];
    if first = last then
      begin
      first:= 0;
      last:= 0;
      end
    else
      first:= (first mod form1.convergencyQueueNumber) + 1;
    end; //else.
end;
////////////////////////////////////////////////////////////////////////////////
function TOverheadQueue.isValidWithAccuracy():Boolean;
  var
    lastIndex, i, counter: Integer;
    dif1, dif2: real;
begin
  result:= false;
  if (first <> 0)
    and (first = (last mod form1.convergencyQueueNumber) + 1)
    and (not(
              (CONVERGENCY_PARAMETER_IS_OVERHEAD) and ((items[last].internalChangedPointsAfterOccurrence = 0) or (items[last].externalChangedPointsAfterOccurrence = 0))
              or ((not CONVERGENCY_PARAMETER_IS_OVERHEAD) and ((items[last].internalOccurred = 0) or (items[last].externalOccurred = 0)))
            )
        ) then
    begin
    result:= true;
    if last < first then
      lastIndex:= form1.convergencyQueueNumber + last
    else
      lastIndex:= last;
    for i:= first to lastIndex do
      begin
      if i > form1.convergencyQueueNumber then
        counter:= i mod form1.convergencyQueueNumber
      else
        counter:= i;
      if CONVERGENCY_PARAMETER_IS_OVERHEAD then
        begin
        if (items[counter].internalChangedPointsAfterOccurrence = 0) or (items[counter].externalChangedPointsAfterOccurrence = 0) then
          begin
          result:= false;
          break;
          end; //if (item....
        dif1:= items[last].externalChangedPointsAfterOccurrence/items[last].internalChangedPointsAfterOccurrence - items[counter].externalChangedPointsAfterOccurrence/items[counter].internalChangedPointsAfterOccurrence;
        dif2:= items[last].internalChangedPointsAfterOccurrence/items[last].externalChangedPointsAfterOccurrence - items[counter].internalChangedPointsAfterOccurrence/items[counter].externalChangedPointsAfterOccurrence;
        if (abs(dif1) >= form1.convergencyAccuracy) or (abs(dif2) >= form1.convergencyAccuracy) then
          begin
          result:= false;
          break;
          end; //if (abs(....
        end //if CONVE....
      else
        begin
        if (items[counter].internalOccurred = 0) or (items[counter].externalOccurred = 0) then
          begin
          result:= false;
          break;
          end; //if (item....
        dif1:= items[last].externalOccurred/items[last].internalOccurred - items[counter].externalOccurred/items[counter].internalOccurred;
        dif2:= items[last].internalOccurred/items[last].externalOccurred - items[counter].internalOccurred/items[counter].externalOccurred;
        if (abs(dif1) >= form1.convergencyAccuracy) or (abs(dif2) >= form1.convergencyAccuracy) then
          begin
          result:= false;
          break;
          end; //if (abs(....
        end; //else of if CONVE....
      end; //for i > M....
    end; //if first....
end;
////////////////////////////////////////////////////////////////////////////////

end.
