unit NerveSimMain;
// -----------------------------------------------
// Nerve Cell Simulation
// (c) J. Dempster, University of Strathclyde 2006
// -----------------------------------------------
// 26/02/06 V1.0
// 12/08/08 V1.1 Lignocaine name changed to Lidocaine

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ScopeDisplay, ValidatedEdit, math, Menus, ExtCtrls,
  HTMLLabel, StrUtils, shared, ComCtrls, htmlhelpviewer, shellapi ;

const
    MaxPoints = 1000000 ;
    MaxDisplayPoints = 1000 ;
    MaxDrugs = 100 ;
    MaxMarkers = 500 ;
    FileHeaderSize = 8192 ;
    DataFileExtension = '.NVS' ;

type

  TDrug = record
          Name : String ;
          ShortName : String ;
          FinalBathConcentration : single ;
          BathConcentration : single ;
          EC50_GNa : Single ;
          EC50_GK : Single ;
          Antagonist : Boolean ;
          end ;

TIon = record
     CIn : Single ;
     COut : Single ;
     FinalCOut : Single ;
     New : Single ;
     G : Single ;
     GMAX : Single ;
     VRev : single ;
     I : Single ;
     m : single ;
     n : single ;
     h : single ;
     end ;

TStimulus = record
          Start : single ;
          Amplitude : single ;
          Duration : single ;
          I : single ;
          end ;

TCell = record
      Temperature : single ;
      rtf : single ;
      Length : single ;
      Radius : single ;
      Area : single ;
      cm : single ;
      c : single ;
      Noise : single ;
      Na : TIon ;
      K : TIon ;
      Cl : TIon ;
      Ca : TIon ;
      Mg : TIon ;
      DAP : TDrug ;
      TTX : TDrug ;
      LIG : TDrug ;
      Stim : TStimulus ;
      Vm : Single ;
      Im : Single ;
      t : Single ;
      dt : single ;
      Step : Integer ;
      NumStepsPerDisplayPoint : Integer ;
      end ;

  TMainFrm = class(TForm)
    ControlsGrp: TGroupBox;
    TissueGrp: TGroupBox;
    GroupBox5: TGroupBox;
    edNaConc: TValidatedEdit;
    Label2: TLabel;
    bNewSaltSoln: TButton;
    AntagonistGrp: TGroupBox;
    cbDrug: TComboBox;
    edDrugConc: TValidatedEdit;
    bAddDrug: TButton;
    Label4: TLabel;
    bNewExperiment: TButton;
    GroupBox6: TGroupBox;
    bStimulusOn: TButton;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    mnLoadExperiment: TMenuItem;
    mnSaveExperiment: TMenuItem;
    N1: TMenuItem;
    mnExit: TMenuItem;
    Timer: TTimer;
    mnEdit: TMenuItem;
    mnCopyData: TMenuItem;
    mnCopyImage: TMenuItem;
    N2: TMenuItem;
    mnPrint: TMenuItem;
    PrinterSetupDialog: TPrinterSetupDialog;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    DisplayGrp: TGroupBox;
    DisplayPage: TPageControl;
    ChartPage: TTabSheet;
    ExperimentPage: TTabSheet;
    sbDisplay: TScrollBar;
    bRecord: TButton;
    bStop: TButton;
    ExptSetup: TImage;
    mnHelp: TMenuItem;
    mnContents: TMenuItem;
    Label6: TLabel;
    edKConc: TValidatedEdit;
    bStandardSaltSoln: TButton;
    Button2: TButton;
    GroupBox1: TGroupBox;
    Label5: TLabel;
    edStimulusCurrent: TValidatedEdit;
    Label3: TLabel;
    edStimulusDuration: TValidatedEdit;
    rbSingleStimulus: TRadioButton;
    rbRepeatedStimulus: TRadioButton;
    RatePanel: TPanel;
    Label1: TLabel;
    edStimulusRate: TValidatedEdit;
    bStimulusOff: TButton;
    scDisplay: TScopeDisplay;
    mnPrinterSetup: TMenuItem;
    procedure FormShow(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
    procedure bRecordClick(Sender: TObject);
    procedure bStopClick(Sender: TObject);
    procedure bNewSaltSolnClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure mnExitClick(Sender: TObject);
    procedure mnCopyDataClick(Sender: TObject);
    procedure mnPrintClick(Sender: TObject);
    procedure mnCopyImageClick(Sender: TObject);
    procedure mnLoadExperimentClick(Sender: TObject);
    procedure bStimulusOnClick(Sender: TObject);
    procedure bStimulationOffClick(Sender: TObject);
    procedure bNewExperimentClick(Sender: TObject);
    procedure mnSaveExperimentClick(Sender: TObject);
    procedure mnContentsClick(Sender: TObject);
    procedure rbSingleStimulusClick(Sender: TObject);
    procedure rbRepeatedStimulusClick(Sender: TObject);
    procedure bStimulusOffClick(Sender: TObject);
    procedure bAddDrugClick(Sender: TObject);
    procedure bStandardSaltSolnClick(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure mnPrinterSetupClick(Sender: TObject);
  private
    { Private declarations }
    ADC : Array[0..MaxPoints-1] of SmallInt ;
    NumPointsInBuf : Integer ;   // No. of data points in buffer
    StartPoint : Integer ;
    NumPointsDisplayed : Integer ;

    // Nerve stimulus
    Cell : TCell ;
    Drugs : Array[0..MaxDrugs-1] of TDrug ;    // Drug properties array
    NumDrugs : Integer ;                     // No. of drugs available

    MarkerList : TStringList ;   // Chart annotation list

    UnsavedData : Boolean ;  // Un-saved data flag

    procedure NewExperiment ;
    function DoNerveSimulationStep : Single ;
    procedure UpdateDisplay( NewPoint : Single ) ;
    procedure AddChartAnnotations ;
    procedure AddDrugMarker( ChartAnnotation : String ) ;
    procedure LoadFromFile( FileName : String ) ;
    procedure SaveToFile( FileName : String ) ;


  public
    { Public declarations }
  end;

var
  MainFrm: TMainFrm;

implementation

{$R *.dfm}

const
    MaxADCValue = 2047 ;
    MinADCValue = -2048 ;
    NoiseStDev = 10 ;
    MaxVm = 200.0 ;
    BackgroundNoiseStDev = 0.1 ;  // Background noise (gms)
    MixingRate = 2000.0 ;

procedure TMainFrm.FormShow(Sender: TObject);
// ------------------------------------------------
// Initialise controls when form is first displayed
// ------------------------------------------------
var
    FileName : String ;
begin

     // Find help file
     Application.HelpFile := ExtractFilePath(ParamStr(0)) + 'nervesim.chm' ;

     // Create annotation list
     MarkerList := TStringList.Create ;

     { Setuo chart display }
     scDisplay.MaxADCValue :=  MaxADCValue ;
     scDisplay.MinADCValue := MinADCValue ;
     scDisplay.DisplayGrid := True ;

     scDisplay.MaxPoints := MaxDisplayPoints ;
     scDisplay.NumPoints := 0 ;
     scDisplay.NumChannels := 1 ;

     { Set channel information }
     scDisplay.ChanOffsets[0] := 0 ;
     scDisplay.ChanUnits[0] := 'mV' ;
     scDisplay.ChanName[0] := 'Vm' ;
     scDisplay.ChanScale[0] := MaxVm / MaxADCValue ;
     scDisplay.yMin[0] := MinADCValue div 2 ;
     scDisplay.yMax[0] := MaxADCValue div 2 ;
     scDisplay.ChanVisible[0] := True ;
     scDisplay.TUnits := 'ms' ;

     scDisplay.xMin := 0 ;
     scDisplay.xMax := scDisplay.MaxPoints-1 ;
     scDisplay.xOffset := 0 ;

     { Create a set of zero level cursors }
     scDisplay.ClearHorizontalCursors ;
     scDisplay.AddHorizontalCursor( 0, clRed, True, '' ) ;
     scDisplay.HorizontalCursors[0] := 0 ;

     // Vertical readout cursor
     scDisplay.ClearVerticalCursors ;
     scDisplay.AddVerticalCursor(-1,clGreen, '?t?y') ;
     scDisplay.VerticalCursors[0] := scDisplay.MaxPoints div 2 ;

     // Start new experiment
     NewExperiment ;

     // Load experiment if file name in parameter string
     FileName := ParamStr(1) ;
     if LowerCase(ExtractFileExt(FileName)) = '.nvs' then begin
        if FileExists(FileName) then LoadFromFile( FileName ) ;
        end ;

     Timer.Enabled := True ;

     end;


procedure TMainFrm.NewExperiment ;
// ------------------------------------
// Start new experiment with new tissue
// ------------------------------------
var
    i : Integer ;
    x : Single ;
begin

     // Create list of drugs
     // --------------------

     NumDrugs := 0 ;

     Drugs[NumDrugs].Name := 'Tetrodotoxin' ;
     Drugs[NumDrugs].ShortName := 'TTX' ;
     Drugs[NumDrugs].FinalBathConcentration := 0.0 ;
     Drugs[NumDrugs].BathConcentration := 0.0 ;
     Drugs[NumDrugs].EC50_GNa := 1E-7*RandG(1.0,0.1) ;
     Drugs[NumDrugs].EC50_GK := 1E3 ;
     Drugs[NumDrugs].Antagonist := True ;
     Inc(NumDrugs) ;

     Drugs[NumDrugs].Name := '3,4 diaminopyridine' ;
     Drugs[NumDrugs].ShortName := 'DAP' ;
     Drugs[NumDrugs].FinalBathConcentration := 0.0 ;
     Drugs[NumDrugs].BathConcentration := 0.0 ;
     Drugs[NumDrugs].EC50_GNa := 1E3 ;
     Drugs[NumDrugs].EC50_GK := 1E-6*RandG(1.0,0.1) ;
     Drugs[NumDrugs].Antagonist := True ;
     Inc(NumDrugs) ;

     Drugs[NumDrugs].Name := 'Lidocaine' ;
     Drugs[NumDrugs].ShortName := 'LID' ;
     Drugs[NumDrugs].FinalBathConcentration := 0.0 ;
     Drugs[NumDrugs].BathConcentration := 0.0 ;
     Drugs[NumDrugs].EC50_GNa := 5E-4 ;
     Drugs[NumDrugs].EC50_GK := 1E3 ;
     Drugs[NumDrugs].Antagonist := True ;
     Inc(NumDrugs) ;

     // Create list of agonists
     cbDrug.Clear ;
     for i := 0 to NumDrugs-1 do
         cbDrug.Items.AddObject( Drugs[i].Name, TObject(i)) ;
     cbDrug.ItemIndex := 0 ;

     Cell.dt := 1E-5 ;

     { Define constant simulation parameters }
     Cell.Temperature := 20.0 ;
     Cell.rtf := 0.02354*(Cell.Temperature + 273.0)/273.0 ;
     Cell.Length := 50.0*1E-4 ;  { cm }
     Cell.Radius := 20.0*1E-4 ; { cm }
     Cell.Area := 2.*PI*Cell.Radius*Cell.Length ;
     Cell.Cm := 1E-6 ; {* Specific membrane capacity F/cm2 }
     Cell.C := Cell.Cm*Cell.Area ;

     { Define initial drug/ion concentrations }
     Cell.Na.Cin := 12. ;                  { Internal [Na] mM }
     Cell.Na.Cout := 145. ;
     Cell.Na.FinalCout := Cell.Na.Cout ;
     Cell.K.Cin := 140. ;         { Internal [K] mM }
     Cell.K.Cout := 5. ;
     Cell.K.FinalCout := Cell.K.Cout ;
     Cell.Cl.Cout := 110.0 ;
     Cell.Cl.Cin := 4.0 ;
     Cell.K.VRev := Cell.rtf * ln( Cell.K.Cout / Cell.K.Cin ) ;
     Cell.Vm := Cell.K.VRev ;              { Set resting potential to K reversal pot. }

     Cell.Ca.Cout := 2.0 ;
     Cell.Mg.Cout := 1. ;
     Cell.Stim.Amplitude := -2E-9 ;
     Cell.Stim.Duration := 1E-3 ;

     Cell.NumStepsPerDisplayPoint := 4 ;
     Cell.Step := 0 ;

     { Clear buffer  }
     for i := 0 to MaxPoints-1 do ADC[i] := 0 ;
     StartPoint :=  0 ;
     scDisplay.SetDataBuf( @ADC[StartPoint] ) ;
     scDisplay.TScale := 1000.0*Cell.dt*Cell.NumStepsPerDisplayPoint ;


     NumPointsDisplayed := 0 ;
     NumPointsInBuf := 0 ;

     // Clear chart annotation
     MarkerList.Clear ;

     bRecord.Enabled := True ;
     bStop.Enabled := False ;

     sbDisplay.Max := MaxDisplayPoints ;
     sbDisplay.Enabled := False ;
     sbDisplay.Position := 0 ;

     bStop.Click ;

     UnSavedData := False ;

     end ;


procedure TMainFrm.TimerTimer(Sender: TObject);
// ---------------------
// Timed event scheduler
// ---------------------
var
    NewPoint : Single ;
begin

     // Ensure that horizontal cursor remains at zero
     scDisplay.HorizontalCursors[0] := 0 ;

     if not bRecord.Enabled then begin
        NewPoint := DoNerveSimulationStep ;
        UpdateDisplay( NewPoint ) ;

        end
     else begin
        // Display
        if scDisplay.XOffset <> sbDisplay.Position then begin
           scDisplay.XOffset := sbDisplay.Position ;
           scDisplay.SetDataBuf( @ADC[sbDisplay.Position] ) ;
           scDisplay.NumPoints := Min( scDisplay.MaxPoints,
                                       sbDisplay.Max - sbDisplay.Position) ;
           // Add annotations to chart
           AddChartAnnotations ;
           scDisplay.Invalidate ;
           end ;
        end ;

     end;


procedure TMainFrm.AddChartAnnotations ;
// -------------------------------------
// Add drug annotations to chart display
// -------------------------------------
var
    i : Integer ;
    MarkerPosition : Integer ;
begin

     scDisplay.ClearMarkers ;
     for i := 0 to MarkerList.Count-1 do begin
         MarkerPosition := Integer(MarkerList.Objects[i]) - scDisplay.XOffset ;
         if (MarkerPosition > 0) and (MarkerPosition < scDisplay.MaxPoints) then begin
            scDisplay.AddMarker( MarkerPosition, MarkerList.Strings[i] ) ;
            end ;
         end ;
     end ;


function TMainFrm.DoNerveSimulationStep : Single ;
{ --------------------------------------
  Run nerve action potential simulation
  --------------------------------------}
type
    TRate = record
          m : single ;
          n : single ;
          h : single ;
          end ;
var
   Alpha,Beta : TRate ;
   dm,dn,dh,dv,mInf,hInf : single ;
   nSteps : Integer ;
   Block : Single ;
   i : Integer ;
   dConc : Single ;
   Sum : Single ;
begin

    { Initialisations at start of simulated sweep }
    if Cell.Step = 0 then begin

       Cell.t := 0. ;
       { Simulation time step }
       Cell.dt := 1E-5 ;
       Cell.Noise := 5E-10 ;

       { Na channels }
       Cell.Na.GMax := 0.12*Cell.Area ; { Max. Na conductance }
       Cell.Na.VRev := Cell.rtf * ln( Cell.Na.Cout / Cell.Na.Cin ) ;

       { K channels }
       Cell.K.VRev := Cell.rtf * ln( Cell.K.Cout / Cell.K.Cin ) ;
       Cell.K.GMax := 0.036*Cell.Area ; { Max. K conductance }

       { Cl channels }
       Cell.Cl.GMax := 0.005*Cell.Area ;    { Chloride conductance }
       { Note. Internal Cl concentration passively determined by
         potassium reversal membrane potential. }
       Cell.Cl.Cin := Cell.Cl.Cout / exp(-Cell.K.Vrev/Cell.rtf) ;
       Cell.Cl.VRev := -Cell.rtf * ln( Cell.Cl.Cout / Cell.Cl.Cin ) ;

       { Nerve Stimulation }

       { Direct current stimulation }
       Cell.Stim.Start := 0.0 ;
       Cell.Stim.Amplitude := 0.0 ;
       Cell.Stim.Duration := 1E-4 ;

       end ;

    dConc := (Cell.Na.FinalCout - Cell.Na.Cout)*MixingRate*Cell.dt ;
    Cell.Na.Cout := Cell.Na.Cout + dConc ;
    Cell.Na.VRev := Cell.rtf * ln( Cell.Na.Cout / Cell.Na.Cin ) ;

    dConc := (Cell.K.FinalCout - Cell.K.Cout)*MixingRate*Cell.dt ;
    Cell.K.Cout := Cell.K.Cout + dConc ;
    Cell.K.VRev := Cell.rtf * ln( Cell.K.Cout / Cell.K.Cin ) ;

    Cell.Cl.Cin := Cell.Cl.Cout / exp(-Cell.K.Vrev/Cell.rtf) + 1.0 ;
    Cell.Cl.VRev := -Cell.rtf * ln( Cell.Cl.Cout / Cell.Cl.Cin ) ;

    // Update drug bath concentrations
    for i := 0 to NumDrugs-1 do begin
         dConc := (Drugs[i].FinalBathConcentration - Drugs[i].BathConcentration)
                  *MixingRate*Cell.dt ;
         Drugs[i].BathConcentration := Drugs[i].BathConcentration + dConc ;
         end ;

    nSteps := Cell.NumStepsPerDisplayPoint ;
    while nSteps > 0 do begin

           { Na current }

           { Activation parameter (m) }
            if( abs( -Cell.Vm - 0.050 ) > 1E-4 ) then begin
                Alpha.m := 1E5*( -Cell.Vm-0.050)/( exp((-Cell.Vm-0.050)/0.01) -1.0) ;
                end
            else begin
                Alpha.m := 5.0*( 1.0/( exp((1E-4)/0.01) -1.0)
                                 + 1.0/( exp((-1E-4)/0.01) -1.0) ) ;
                end ;

            Beta.m := 4E3*exp( -(Cell.Vm + 0.06)/0.018 ) ;
            if Cell.Step = 0 then Cell.Na.m := Alpha.m / ( Alpha.m + Beta.m ) ;
            dm := ( Alpha.m*(1.0 - Cell.Na.m) - Beta.m*Cell.Na.m )*Cell.dt ;
            Cell.Na.m := Cell.Na.m + dm ;

            { Inactivation parameter (h) }
            Alpha.h := 70.0*exp( -(Cell.Vm + 0.06)/0.02) ;
            Beta.h := 1E3/( exp((-Cell.Vm - 0.03)/0.01) + 1.0) ;
            if Cell.Step = 0 then Cell.Na.h := Alpha.h / ( Alpha.h + Beta.h ) ;

            dh := ( Alpha.h*(1.0 - Cell.Na.h) - Beta.h*Cell.Na.h )*Cell.dt ;
            Cell.Na.h := Cell.Na.h + dh ;

            { TTX dissociation constant 5nM (from Hille, Ionic Channels ..) }

            // Fractional Na channel block
            Sum := 0.0 ;
            for i := 0 to NumDrugs-1 do begin
                Sum := Sum + (Drugs[i].BathConcentration/Drugs[i].EC50_GNa) ;
                end ;

            outputdebugString(PChar(format('%.4g',[sum]))) ;
            Block := 1.0 / (1.0 + Sum ) ;
            Cell.Na.G := Cell.Na.m*Cell.Na.m*Cell.Na.m*Cell.Na.h*Cell.Na.GMax*Block ;
            Cell.Na.I := Cell.Na.g*( Cell.Vm - Cell.Na.VRev ) ;

            { Potassium current }

            { Activation parameter (n) }
            if abs(-Cell.Vm - 0.05) > 1E-4  then begin
                Alpha.n := 1E4*(-Cell.Vm - 0.05)/( exp((-Cell.Vm - 0.05)/0.01) -1.0) ;
                end
            else begin
                Alpha.n := 0.5*( 1.0/( exp(1E-2) -1.0) -1.0/( exp(-1E-2) -1.0) );
                end ;
            Beta.n := 125.0*exp( (-Cell.Vm - 0.06)/0.08 ) ;
            if Cell.Step = 0 then Cell.K.n := Alpha.n / ( Alpha.n + Beta.n ) ;

            dn := ( Alpha.n*(1.0 - Cell.K.n) - Beta.n*Cell.K.n )*Cell.dt ;
            Cell.K.n := Cell.K.n + dn ;

            // Fractional K channel block
            Sum := 0.0 ;
            for i := 0 to NumDrugs-1 do begin
                Sum := Sum + (Drugs[i].BathConcentration/Drugs[i].EC50_GK) ;
                end ;
            Block := 1.0 / (1.0 + Sum ) ;

            Cell.K.G := Cell.K.n*Cell.K.n*Cell.K.n*Cell.K.n*Cell.K.gMax*Block ;
            Cell.K.I := Cell.K.g * (Cell.Vm - Cell.K.VRev ) ;

            { Leak current (chloride) }

            Cell.Cl.I := Cell.Cl.GMAX * (Cell.Vm - Cell.Cl.VRev ) ;

            { Direct muscle stimulation }
            if not bStimulusOn.Enabled then begin
               if Cell.t >= Cell.Stim.Start then begin
                  Cell.Stim.I := Cell.Stim.Amplitude ;
                  end ;
               if Cell.t >= (Cell.Stim.Start + Cell.Stim.Duration) then begin
                  Cell.Stim.I := 0. ;
                  if rbRepeatedStimulus.Checked then begin
                     Cell.Stim.Start := Cell.Stim.Start + (1.0/edStimulusRate.Value) ;
                     end
                  else begin
                     bStimulusOn.Enabled := True ;
                     bStimulusOff.Enabled := False ;
                     end ;
                  end ;
               end
            else begin
               Cell.Stim.I := 0. ;
               end ;

            { Compute membrane current }
            Cell.Im := Cell.Na.I + Cell.K.I + Cell.Cl.I - Cell.Stim.I
                       + (random - 0.5)*Cell.Noise ;

            { Compute change in membrane potential }

            dv := ( -Cell.Im ) * Cell.dt/Cell.C ;
            Cell.Vm := Cell.Vm + dv ;

            Inc(Cell.Step) ;
            Cell.t := Cell.t + Cell.dt ;
            Dec(nSteps) ;
            
            end ;

     Result := Cell.Vm ;

     end ;


procedure TMainFrm.UpdateDisplay(
           NewPoint : Single ) ;
// -------------------
// Update chart display
// -------------------
var
    StartPoints : Integer ;
begin

    if NumPointsDisplayed >= MaxDisplayPoints then begin
       StartPoints := MaxDisplayPoints div 10 ;
       NumPointsDisplayed := StartPoints ;
       sbDisplay.Position := NumPointsInBuf - StartPoints + 1 ;
       scDisplay.XOffset := sbDisplay.Position ;
       scDisplay.SetDataBuf( @ADC[sbDisplay.Position] ) ;
       sbDisplay.Max := sbDisplay.Max + MaxDisplayPoints ;
       // Add annotations to chart
       AddChartAnnotations ;
       end ;

    ADC[NumPointsInBuf] := Round( (NewPoint*1000)/scDisplay.ChanScale[0] ) ;
    Inc(NumPointsInBuf) ;
    Inc(NumPointsDisplayed) ;
    scDisplay.DisplayNewPoints( NumPointsInBuf - scDisplay.XOffset ) ;
    scDisplay.Invalidate ;

    end ;


procedure TMainFrm.bRecordClick(Sender: TObject);
// ----------------
// Start simulation
// ----------------
begin
     bRecord.Enabled := False ;
     bStop.Enabled := True ;
     sbDisplay.Enabled := False ;
     bNewExperiment.Enabled := False ;
     TissueGrp.Enabled := False ;
     bStimulusOff.Enabled := False ;
     bStimulusOn.Enabled := True ;

     UnSavedData := True ;

     NumPointsDisplayed := 0 ;
     sbDisplay.Position := NumPointsInBuf + 1 ;
     scDisplay.XOffset := sbDisplay.Position ;
     scDisplay.SetDataBuf( @ADC[sbDisplay.Position] ) ;
     sbDisplay.Max := sbDisplay.Max + MaxDisplayPoints ;
     // Add annotations to chart
     AddChartAnnotations ;

     end;


procedure TMainFrm.bStopClick(Sender: TObject);
// ----------------
// Stop simulation
// ----------------
begin
     bRecord.Enabled := True ;
     bStop.Enabled := False ;
     sbDisplay.Enabled := True ;
     bNewExperiment.Enabled := True ;
     TissueGrp.Enabled := True ;
     bStimulusOn.Enabled := False ;
     bStimulusOff.Enabled := False ;

     end;


procedure TMainFrm.bNewSaltSolnClick(Sender: TObject);
// -------------------------------------
// Change concentration of ions in bath
// -------------------------------------
var
     ChartAnnotation : String ;
begin

     Cell.Na.FinalCout := edNaConc.Value ;
     Cell.K.FinalCout := edKConc.Value ;

     // Add chart annotation
     ChartAnnotation := format('[Na]=%3g, [K]=%3g mM',
                        [Cell.Na.FinalCout,Cell.K.FinalCout]) ;
     AddDrugMarker( ChartAnnotation ) ;

     end;


procedure TMainFrm.FormResize(Sender: TObject);
// ------------------------------------------------------
// Set control size/locations when program window resized
// ------------------------------------------------------
begin

     ControlsGrp.Height := Max( ClientHeight - ControlsGrp.Top - 10,2 ) ; ;

     DisplayGrp.Height := Max( ClientHeight - DisplayGrp.Top - 10,2) ;
     DisplayGrp.Width := Max( ClientWidth - DisplayGrp.Left - 10,2) ;

     DisplayPage.Height := DisplayGrp.ClientHeight - DisplayPage.Top - 10 ;
     DisplayPage.Width := DisplayGrp.ClientWidth - DisplayPage.Left - 10 ;

     bRecord.Top := ChartPage.ClientHeight - bRecord.Height - 5 ;
     bStop.Top := bRecord.Top ;

     sbDisplay.Top := bRecord.Top -  sbDisplay.Height - 2 ;
     sbDisplay.Width := Max( ChartPage.ClientWidth - sbDisplay.Left - 10,2) ;

     scDisplay.Height := Max( sbDisplay.Top - scDisplay.Top,2) ;
     scDisplay.Width := Max( ChartPage.ClientWidth - scDisplay.Left - 10,2) ;

     // Centre experiment setup pictures on page

     ExptSetup.Left := Max( (ExperimentPage.ClientWidth - ExptSetup.Width) div 2,4) ;
     ExptSetup.Top := Max( (ExperimentPage.ClientHeight - ExptSetup.Height) div 2,4) ;

     end;


procedure TMainFrm.AddDrugMarker(
          ChartAnnotation : String
          ) ;
// ------------------------------
// Add drug addition/wash marker
// ------------------------------
begin
     if MarkerList.Count < MaxMarkers then begin
        ChartAnnotation := AnsiReplaceStr( ChartAnnotation, '-00', '-' ) ;
        ChartAnnotation := AnsiReplaceStr( ChartAnnotation, '00E', '0E' ) ;
        MarkerList.AddObject( ChartAnnotation, TObject(NumPointsInBuf) ) ;
        scDisplay.AddMarker( NumPointsInBuf - scDisplay.XOffset, ChartAnnotation ) ;
        end ;
     end ;


procedure TMainFrm.mnCopyDataClick(Sender: TObject);
// ----------------------------------------------------
// Copy sample values of displayed signals to clipboard
// ----------------------------------------------------
begin
     scDisplay.CopyDataToClipboard ;
     end;


procedure TMainFrm.mnPrintClick(Sender: TObject);
// ---------------------------
// Print displayed chart trace
// ---------------------------
begin
    scDisplay.PrinterLeftMargin := 25 ;
    scDisplay.PrinterRightMargin := 25 ;
    scDisplay.PrinterTopMargin := 25 ;
    scDisplay.PrinterBottomMargin := 25 ;
    scDisplay.TCalBar := 1.0 / scDisplay.TScale ;
    scDisplay.ChanCalBar[0] := 10.0 ;
    scDisplay.Print ;
    end;


procedure TMainFrm.mnPrinterSetupClick(Sender: TObject);
// -------------
// Setup printer
// -------------
begin
     PrinterSetupDialog.Execute ;
     end;

procedure TMainFrm.SaveToFile(
          FileName : String
          ) ;
// ----------------------------
// Save chart recording to file
// ----------------------------
var
   Header : array[1..FileHeaderSize] of ansichar ;
   i : Integer ;
   FileHandle : Integer ;
begin

     FileHandle := FileCreate( FileName ) ;
     if FileHandle < 0 then Exit ;

     { Initialise empty header buffer with zero bytes }
     for i := 1 to sizeof(Header) do Header[i] := chr(0) ;

     AppendInt( Header, 'NPOINTS=', NumPointsInBuf ) ;

     AppendInt( Header, 'NMARKERS=', MarkerList.Count ) ;
     for i := 0 to MarkerList.Count-1 do begin
         AppendInt( Header, format('MKPOINT%d=',[i]), Integer(MarkerList.Objects[i])) ;
         AppendString( Header, format('MKTEXT%d=',[i]), MarkerList.Strings[i] ) ;
         end ;

     // Write header
     FileWrite( FileHandle, Header, SizeOf(Header)) ;
     // Write chart data
     FileWrite( FileHandle, ADC, NumPointsInBuf*2 ) ;
     // Close file
     FileClose( FileHandle ) ;

     UnSavedData := False ;
     end ;


procedure TMainFrm.LoadFromFile(
          FileName : String
          ) ;
// ------------------------------
// Load chart recording from file
// ------------------------------
var
   Header : array[1..FileHeaderSize] of ansichar ;
   i : Integer ;
   FileHandle : Integer ;
   NumMarkers : Integer ;
   MarkerPoint : Integer ;
   MarkerText : String ;
begin

     NumPointsInBuf := 0 ;

     FileHandle := FileOpen( FileName, fmOpenRead ) ;
     if FileHandle < 0 then Exit ;

     FileSeek( FileHandle, 0, 0 ) ;

     // Read header
     if FileRead(FileHandle, Header, Sizeof(Header)) = Sizeof(Header) then begin

        NewExperiment ;

        ReadInt( Header, 'NPOINTS=', NumPointsInBuf ) ;

        ReadInt( Header, 'NMARKERS=', NumMarkers ) ;
        MarkerList.Clear ;
        for i := 0 to NumMarkers-1 do begin
            ReadInt( Header, format('MKPOINT%d=',[i]), MarkerPoint) ;
            ReadString( Header, format('MKTEXT%d=',[i]), MarkerText ) ;
            MarkerList.AddObject( MarkerText, TObject(MarkerPoint)) ;
            end ;
        end ;

     if NumPointsInBuf > 0 then begin
        FileRead( FileHandle, ADC, NumPointsInBuf*2 ) ;
        end ;

     // Close data file
     FileClose( FileHandle ) ;

     UnsavedData := False ;
     scDisplay.XOffset := -1 ;
     sbDisplay.Position := 0 ;
     sbDisplay.Max := Max(NumPointsInBuf,1) ;
     scDisplay.Invalidate ;

     end ;



procedure TMainFrm.mnCopyImageClick(Sender: TObject);
// -----------------------------------------
// Copy image of displayed trace to clipboad
// -----------------------------------------
begin
     scDisplay.MetafileWidth := 600 ;
     scDisplay.MetafileHeight := 400 ;
     scDisplay.TCalBar := 1.0/scDisplay.TScale ;
     scDisplay.ChanCalBar[0] := 10.0 ;
     scDisplay.CopyImageToClipBoard ;
     end;

procedure TMainFrm.mnLoadExperimentClick(Sender: TObject);
// -------------------------
// Load experiment from file
// -------------------------
begin


    if UnSavedData then begin
        if MessageDlg('Existing experiment will be overwritten! Are you sure?', mtConfirmation,
           [mbYes,mbNo],0) = mrNo then Exit ;
        end ;

   OpenDialog.options := [ofPathMustExist] ;
   OpenDialog.FileName := '' ;
   OpenDialog.DefaultExt := DataFileExtension ;
   //OpenDialog.InitialDir := OpenDirectory ;
   OpenDialog.Filter := format( ' Nerve Expt. (*%s)|*%s',
                                [DataFileExtension,DataFileExtension]) ;
   OpenDialog.Title := 'Load Experiment ' ;

   // Open selected data file
   if OpenDialog.execute then LoadFromFile( OpenDialog.FileName ) ;

   end;

procedure TMainFrm.bStimulusOnClick(Sender: TObject);
// --------------------
// Start nerve stimulus
// --------------------
begin
     bStimulusOn.Enabled := False ;
     bStimulusOff.Enabled := True ;

     Cell.Stim.Start := Cell.t ;
     Cell.Stim.Duration := edStimulusDuration.Value ;
     Cell.Stim.Amplitude := edStimulusCurrent.Value ;

     end;


procedure TMainFrm.bStimulationOffClick(Sender: TObject);
// --------------------
// Start nerve stimulus
// --------------------
begin
     bStimulusOn.Enabled := True ;
     bStimulusOff.Enabled := False ;
     end;


procedure TMainFrm.bNewExperimentClick(Sender: TObject);
// ---------------------
// Select new experiment
// ---------------------
begin
     if UnSavedData then begin
        if MessageDlg('Existing experiment will be erased! Are you sure?', mtConfirmation,
           [mbYes,mbNo],0) = mrYes then NewExperiment ;
        end
     else NewExperiment ;
     end;


procedure TMainFrm.mnSaveExperimentClick(Sender: TObject);
// -----------------------
// Save experiment to file
// -----------------------
begin

     { Present user with standard Save File dialog box }
     SaveDialog.options := [ofHideReadOnly,ofPathMustExist] ;
     SaveDialog.FileName := '' ;
     SaveDialog.DefaultExt := DataFileExtension ;
     SaveDialog.Filter := format( '  Nerve Expt. (*%s)|*%s',
                                  [DataFileExtension,DataFileExtension]) ;
     SaveDialog.Title := 'Save Experiment' ;

     if SaveDialog.Execute then SaveToFile( SaveDialog.FileName ) ;

     end ;


procedure TMainFrm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
// -----------------------------------------------
// Check whether user really wants to stop program
// -----------------------------------------------
begin
     if MessageDlg('Stop NerveSim Program! Are you sure?', mtConfirmation,
           [mbYes,mbNo],0) = mrYes then CanClose := True
                                   else CanClose := False ;
     end;


procedure TMainFrm.mnExitClick(Sender: TObject);
// ------------
// Stop Program
// ------------
begin
     Close ;
     end;


procedure TMainFrm.mnContentsClick(Sender: TObject);
// -----------------------
//  Help/Contents menu item
//  -----------------------
//begin
//     application.HelpCommand( 10 ) ;
// -----------------------
//  Help/Contents menu item
//  -----------------------
var
   pWinDir : PChar ;
   WindowsDir,FileName : String ;
begin


     GetMem( pWinDir, 512 ) ;
     GetWindowsDirectoryW( pWinDir, 256 ) ;
     FileName := pWinDir + '\hh.exe' ;

     ShellExecute( Handle,PChar('open'),
                   PChar(FileName),
                   PChar(Application.HelpFile),
                   nil,
                   SW_SHOWNORMAL) ;
     FreeMem( pWinDir ) ;

     end;


procedure TMainFrm.rbSingleStimulusClick(Sender: TObject);
begin
     RatePanel.Visible := False ;
     end;


procedure TMainFrm.rbRepeatedStimulusClick(Sender: TObject);
begin
     RatePanel.Visible := True ;
     end;


procedure TMainFrm.bStimulusOffClick(Sender: TObject);
begin
     bStimulusOn.Enabled := True ;
     bStimulusOff.Enabled := False ;
     end;


procedure TMainFrm.bAddDrugClick(Sender: TObject);
var
    iDrug : Integer ;
    ChartAnnotation : String ;
begin
     iDrug :=  Integer(cbDrug.Items.Objects[cbDrug.ItemIndex]) ;
     Drugs[iDrug].FinalBathConcentration := Drugs[iDrug].FinalBathConcentration
                                            + edDrugConc.Value ;

     // Add chart annotation
     ChartAnnotation := format('%s= %.3g M',
     [Drugs[iDrug].ShortName,Drugs[iDrug].FinalBathConcentration]) ;
     AddDrugMarker( ChartAnnotation ) ;

      end;


procedure TMainFrm.bStandardSaltSolnClick(Sender: TObject);
begin
     edNaConc.Value := 120.0 ;
     edKConc.Value := 5.0 ;
     bNewSaltSoln.Click ;
     end;


procedure TMainFrm.Button2Click(Sender: TObject);
// --------------------------
// Remove all drugs from bath
// --------------------------
var
    i : Integer ;
    ChartAnnotation : String ;
begin
     ChartAnnotation := '' ;
     for i := 0 to NumDrugs do begin
         if Drugs[i].FinalBathConcentration > 0.0 then begin
            if ChartAnnotation <> '' then ChartAnnotation := ChartAnnotation + ', ' ;
            ChartAnnotation := ChartAnnotation + Drugs[i].ShortName + '=0' ;
            end ;
         Drugs[i].FinalBathConcentration := 0.0 ;
         end ;
     if ChartAnnotation <> '' then begin
        ChartAnnotation := ChartAnnotation + ' M' ;
        AddDrugMarker( ChartAnnotation ) ;
        end ;

     end;

end.
