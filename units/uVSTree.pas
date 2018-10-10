{***
* Extensão do VirtualString Tree
* Autor: Carlos Gonzaga
* Data: 12.12.2016
*}

unit uVSTree;

interface

uses Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtDlgs, ImgList, ExtCtrls, ComCtrls, Buttons, Mask,
  VirtualTrees;


type
  TVirtualStringTree =class(VirtualTrees.TVirtualStringTree)
  private
    _alternateRowColor: TColor;
    function getIndexItem: Integer ;
    procedure setIndexItem(const AIndex: Integer);
//    procedure DoPaintAlternateRowColor();
  public
    property IndexItem: Integer read getIndexItem write setIndexItem ;
    property AlternateRowColor: TColor read _alternateRowColor write _alternateRowColor;

    procedure DoCellPaint(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
  end;



implementation

uses Types ;


{ TVirtualStringTree }

procedure TVirtualStringTree.DoCellPaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
var
  C: TCanvas;
begin
    if Assigned(Node) then
    begin
        C :=TargetCanvas ;
        //seleciona row x col
        if Node = Sender.FocusedNode then
        begin
            if Column <> Sender.FocusedColumn then
                C.Brush.Color :=clGradientActiveCaption
            else
                C.Brush.Color :=Self.Colors.FocusedSelectionColor;
        end
        //alterna cor
        else begin
            if Column <> Sender.FocusedColumn then
            begin
//                if Odd(Node.Index) then
//                    C.Brush.Color :=Self.AlternateRowColor;
            end
            else
                C.Brush.Color :=clGradientActiveCaption;
        end;
        C.FillRect(CellRect);
    end;
end;

function TVirtualStringTree.getIndexItem: Integer;
var
  P: PVirtualNode ;
begin
    P :=Self.GetFirstSelected() ;
    if P <> nil then  Result :=P.Index
    else              Result :=-1;
end;

procedure TVirtualStringTree.setIndexItem(const AIndex: Integer);
var
  P: PVirtualNode ;
begin
    P :=Self.GetFirst();
    repeat
        if P.Index = AIndex then
        begin
            Self.Selected[P] :=True;
            Break ;
        end;
        P :=Self.GetNext(P) ;
    until P = nil ;
end;



end.
