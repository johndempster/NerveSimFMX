program NerveSim;

uses
  System.StartUpCopy,
  FMX.Forms,
  FMX.Types,
  NerveSimMain in 'NerveSimMain.pas' {MainFrm},
  ModalBox in 'ModalBox.pas' {ModalBoxFrm},
  NerveModel in 'NerveModel.pas' {Model: TDataModule}
  {$IFDEF MACOS} ,FMX.Platform.Mac in 'FMX.Platform.Mac.pas' {$ENDIF};

{$R *.res}

begin
  {$IFDEF MACOS}
  GlobalUseMetal := true ;
  {$ENDIF}
  Application.Initialize;
  Application.CreateForm(TMainFrm, MainFrm);
  Application.CreateForm(TModalBoxFrm, ModalBoxFrm);
  Application.CreateForm(TModel, Model);
  Application.Run;
end.
