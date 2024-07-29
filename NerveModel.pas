unit NerveModel;
// -------------
// Tissue models
// -------------
// 28.01.22 Model code moved from ObSImMain to ObSimModel
// 18.04.22 Rat diaphragm model (from Twitch simulation) added

interface

uses
  System.SysUtils, System.Classes, System.Math, System.strutils, FMX.ListBox ;


const
    MaxDrugs = 100 ;
    MixingRate = 2000.0 ;

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
          Active : Boolean ;
          Repeated : Boolean ;
          Rate : Single ;
          end;

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

  TModel = class(TDataModule)
  private
    { Private declarations }
  public
    { Public declarations }
    Cell : TCell ;
    Drugs : Array[0..MaxDrugs-1] of TDrug ;    // Drug properties array
    NumDrugs : Integer ;                       // No. of drugs available

    procedure Initialize ;
    procedure GetListOfDrugs(
              DrugList : TComboBox ) ;         // Return list of drugs
    function DoSimulationStep : Single ;

  end;

var
  Model: TModel;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}


procedure TModel.Initialize ;
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

     Cell.dt := 2E-5 ;

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

     Cell.NumStepsPerDisplayPoint := 6 ;
     Cell.Step := 0 ;

     end ;

procedure TModel.GetListOfDrugs(
          DrugList : TComboBox  ) ;        // Type of drug (Agonist,Antagonist,Unknown)
// ---------------------------------------
// Return list of drugs of specified type
// ---------------------------------------
var
    i : Integer ;
begin

     DrugList.Clear ;

     for i := 0 to NumDrugs-1 do
         begin
         DrugList.Items.AddObject( Drugs[i].Name, TObject(i)) ;
         end;

     DrugList.ItemIndex := 0 ;

     end ;

function TModel.DoSimulationStep : Single ;
{ -------------------------------------------------------
  Run nerve action potential simulation for one time step
  -------------------------------------------------------}
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
       //Cell.dt := 2E-5 ;
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

//            outputdebugString(PChar(format('%.4g',[sum]))) ;
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

            { Stimulate nerve }
            if Cell.Stim.Active then
               begin
               if Cell.t >= Cell.Stim.Start then
                  begin
                  Cell.Stim.I := Cell.Stim.Amplitude ;
                  end ;
               if Cell.t >= (Cell.Stim.Start + Cell.Stim.Duration) then
                  begin
                  Cell.Stim.I := 0. ;
                  if Cell.Stim.Repeated then
                     begin
                     Cell.Stim.Start := Cell.Stim.Start + (1.0/Cell.Stim.Rate) ;
                     end
                  else begin
                     Cell.Stim.Active := False ;
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


end.
