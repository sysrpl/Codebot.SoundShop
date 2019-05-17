program SoundShop;

{$mode delphi}

uses
  {$ifdef unix}
  CThreads,
  {$endif}
  Interfaces,
  Forms, SDL2, Music, Scaling, Piano, Audio, Main, Recording
  { Add units here };

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Scaled := True;
  Application.Initialize;
  Application.CreateForm(TMusicalForm, MusicalForm);
  Application.Run;
end.

