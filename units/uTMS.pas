unit uTMS;

interface

uses Windows, Messages, SysUtils, Classes, Graphics, Controls,
  AdvOfficeStatusBar, AdvOfficeStatusBarStylers;

type
  TAdvOfficeStatusBar = class(AdvOfficeStatusBar.TAdvOfficeStatusBar)
  private
  public
    constructor Create(aOwner: TComponent); override;
  end;


implementation

{ TAdvOfficeStatusBar }

constructor TAdvOfficeStatusBar.Create(aOwner: TComponent);
begin
    inherited Create(aOwner);

end;

end.
