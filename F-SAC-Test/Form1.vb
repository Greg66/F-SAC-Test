'Implementation of F-SAC model
'Copyright: Gregor Reichert, 2013
'
'Literature:
'Functional-Segment Activity Coefficient Model. 1. Model Formulation, Rafael de Soares, et. al, Ind. Eng. Chem. Res. 2013, 52, 11159-11171
'Functional-Segment Activity Coefficient Model. 2. Associating Mixtures, Rafael de Soares, et. al, Ind. Eng. Chem. Res. 2013, 52, 11172-11181

'Soares and Gerber (2013), Ind. Eng. Chem. Res. DOI:10.1021/ie400170a

'Program adapted from Fortran code at supplementary information download page

Imports System.IO
Imports System.Math


Public Class Form1
    Private Subgrouplines(), GroupLines(), Componentlines(), ChemSepLines() As String
    Protected FSubGroups As System.Collections.Generic.Dictionary(Of Integer, FSAC_Subgroup)
    Protected FGroups As System.Collections.Generic.Dictionary(Of Integer, FSAC_Group)
    Protected FComponents As System.Collections.Generic.Dictionary(Of Integer, FSAC_Component)
    Protected CSComponents As System.Collections.Generic.Dictionary(Of String, ChemSep_Component)
    Protected DeltaW(1, 1, 21, 21), DeltaW_HB(1, 1, 21, 21) As Double
    Protected SegGamma(21, 1), SegGammaOld(21, 1) As Double
    Protected SegGammaPR(21, 1) As Double
    Protected ProfileMatrix(21, 1) As Double
    Protected FSAC_CompData(1) As FSAC_CompData
    Protected FSAC_Comps(1) As FSAC_Component
    Protected ChemSep_Comps(1) As ChemSep_Component


    'Define number of components. This program supports only 2 components!
    Dim Comps As Integer = 2

    'FSAC-Model Constants
    Dim FSAC_FPOL As Double = 1
    Dim FSAC_COMPSEG As Integer = 51
    Dim FSAC_E0 As Double = 0.0002395
    Dim FSAC_RGAS As Double = 0.001987
    Dim FSAC_RAV As Double = 1.07
    Dim FSAC_P As Double = 0.75
    Dim FSAC_R As Double = 66.69
    Dim FSAC_Q As Double = 50
    Dim FSAC_AEFPRIME As Double = PI * FSAC_P ^ 2
    Dim FSAC_ALPHA As Double = (0.3 * FSAC_AEFPRIME ^ 1.5) / FSAC_E0
    Dim FSAC_ALPHAPRIME As Double = FSAC_FPOL * FSAC_ALPHA

    Public Sub New()

        ' This call is necessary for Designer
        InitializeComponent()

        ' Initialisation of program
        Dim pathsep = System.IO.Path.DirectorySeparatorChar
        Dim filepath As String = My.Application.Info.DirectoryPath & pathsep

        'read model data
        Subgrouplines = IO.File.ReadAllLines(filepath & "FSCAC-Subgroups.csv")
        GroupLines = IO.File.ReadAllLines(filepath & "FSAC-groups.csv")
        Componentlines = IO.File.ReadAllLines(filepath & "FSCAC-Components.csv")
        ChemSepLines = IO.File.ReadAllLines(filepath & "ChemSep-Components.csv")

        'initialize F-SAC subgroups
        'Group; GroupID; SubGroup; SubGroupID; MW;	Rk;	Qk;	Fixed;	Comment
        FSubGroups = New System.Collections.Generic.Dictionary(Of Integer, FSAC_Subgroup)
        For i = 1 To Subgrouplines.Length - 1
            With Subgrouplines(i)
                Dim SG As New FSAC_Subgroup
                SG.Group = .Split(";")(0)
                SG.GID = .Split(";")(1)
                SG.Subgroup = .Split(";")(2)
                SG.SGID = .Split(";")(3)
                SG.MW = .Split(";")(4)
                SG.Rk = .Split(";")(5)
                SG.Qk = .Split(";")(6)
                FSubGroups.Add(SG.SGID, SG)
            End With
        Next

        'initialize F-SAC main groups
        'Group;GroupID;charge;Q+;Q-;sigma+;HBAcc;HBDonn;fixed
        FGroups = New System.Collections.Generic.Dictionary(Of Integer, FSAC_Group)
        For i = 1 To GroupLines.Length - 3
            With GroupLines(i)
                Dim GR As New FSAC_Group
                GR.Group = .Split(";")(0)
                GR.GID = .Split(";")(1)
                GR.Charge = .Split(";")(2)
                GR.Qplus = .Split(";")(3)
                GR.Qminus = .Split(";")(4)
                GR.Sigma_pos = .Split(";")(5)
                GR.HBAcc = .Split(";")(6)
                GR.HBDon = .Split(";")(7)
                FGroups.Add(GR.GID, GR)
            End With
        Next

        'initialize F-SAC component List
        'ID;Name;CAS;formula;MW;Fixed;Count;SubGroup1;nu1;SubGroup2;nu2;SubGroup3;nu3;SubGroup4;nu4;;
        FComponents = New System.Collections.Generic.Dictionary(Of Integer, FSAC_Component)
        For i = 1 To Componentlines.Length - 1
            With Componentlines(i)
                Dim Comp As New FSAC_Component
                Comp.ID = .Split(";")(0)
                Comp.Name = .Split(";")(1)
                Comp.CAS = .Split(";")(2)
                Comp.Formula = .Split(";")(3)
                Comp.MW = CheckDouble(.Split(";")(4))
                Comp.Count = .Split(";")(6)
                Comp.SG(0) = CheckDouble(.Split(";")(7))
                Comp.GC(0) = CheckDouble(.Split(";")(8))
                Comp.SG(1) = CheckDouble(.Split(";")(9))
                Comp.GC(1) = CheckDouble(.Split(";")(10))
                Comp.SG(2) = CheckDouble(.Split(";")(11))
                Comp.GC(2) = CheckDouble(.Split(";")(12))
                Comp.SG(3) = CheckDouble(.Split(";")(13))
                Comp.GC(3) = CheckDouble(.Split(";")(14))
                FComponents.Add(Comp.ID, Comp)

            End With
        Next

        'initialize ChemSep component List
        'Component;MW;Formula;CAS;UNIFAC;MODFAC;Formula ID;A;B;C;D;E;T min;T max;
        CSComponents = New System.Collections.Generic.Dictionary(Of String, ChemSep_Component)
        For i = 1 To ChemSepLines.Length - 1
            With ChemSepLines(i)
                Dim Comp As New ChemSep_Component
                Comp.Name = .Split(";")(0)
                Comp.MW = CheckDouble(.Split(";")(1))
                Comp.Formula = .Split(";")(2)
                Comp.CAS = .Split(";")(3)
                Comp.UNIFAC = .Split(";")(4)
                Comp.MODFAC = .Split(";")(5)
                Comp.A = .Split(";")(7)
                Comp.B = .Split(";")(8)
                Comp.C = .Split(";")(9)
                Comp.D = .Split(";")(10)
                Comp.E = .Split(";")(11)
                Comp.TMin = .Split(";")(12)
                Comp.TMax = .Split(";")(13)
                CSComponents.Add(Comp.CAS, Comp)
            End With
        Next

        'Fill CompSelection Dropdown lists
        'only components are displayed which are available in both F-SAC and ChemSep database
        'Component CAS-number is used as index for searching both databases
        'ChemSep database delivers vapour pressure model parameters
        For Each f In FComponents
            Dim FComp As FSAC_Component = f.Value
            If CSComponents.ContainsKey(FComp.CAS) Then
                CB_Comp1.Items.Add(FComp.Name & "    > " & FComp.CAS)
                CB_Comp2.Items.Add(FComp.Name & "    > " & FComp.CAS)
            End If
        Next


        'initiaise component list
        For i = 0 To 1
            FSAC_CompData(i) = New FSAC_CompData
        Next
        
    End Sub
    Private Function CheckDouble(ByVal V As String) As Double
        If Not V = "" Then
            Return V
        Else
            Return Nothing
        End If
    End Function
  
    Private Sub FillReportBox(ByVal SelText As String, ByVal CompIDX As Integer, ByVal RB As TextBox)
        'write component data to infobox and save component to component list

        Dim CAS As String 'CAS-number
        Dim k As Integer

        RB.Clear()

        If SelText <> "" Then
            CAS = SelText.Split(">")(1)
            CAS = CAS.TrimStart(" ")

            'search F-SAC database for component with identical CAS-number
            'and save data into components list
            For Each Comp In FComponents
                If Comp.Value.CAS = CAS Then 'component found
                    FSAC_Comps(CompIDX) = Comp.Value
                    Exit For
                End If
            Next

            If CSComponents.ContainsKey(FSAC_Comps(CompIDX).CAS) Then
                'this is redundant as only components available in ChemSep database are offered to select in drop down list

                ChemSep_Comps(CompIDX) = CSComponents.Item(FSAC_Comps(CompIDX).CAS)
                Dim FSG As FSAC_Subgroup

                With RB

                    'report component data
                    .AppendText("ChemSep - Database" & vbCrLf)
                    .AppendText("Name : " & ChemSep_Comps(CompIDX).Name & vbCrLf)
                    .AppendText("CAS : " & ChemSep_Comps(CompIDX).CAS & vbCrLf)
                    .AppendText("Formula : " & ChemSep_Comps(CompIDX).Formula & vbCrLf)
                    .AppendText("UNIFAC : " & ChemSep_Comps(CompIDX).UNIFAC & vbCrLf)
                    .AppendText(vbCrLf & "================" & vbCrLf)
                    .AppendText("FSAC - Subgroups:" & vbCrLf)

                    'report all subgroups
                    For k = 0 To FSAC_Comps(CompIDX).Count - 1
                        If FSAC_Comps(CompIDX).GC(k) > 0 Then
                            FSG = FSubGroups.Item(FSAC_Comps(CompIDX).SG(k))
                            .AppendText("Group" & k & ": " & FSAC_Comps(CompIDX).GC(k) & " x " & FSG.Subgroup & " (" & FSAC_Comps(CompIDX).SG(k) & ")" & vbCrLf)
                        End If
                    Next
                End With
            End If
        End If
    End Sub
    Private Sub CB_Comp1_SelectedIndexChanged(sender As System.Object, e As System.EventArgs) Handles CB_Comp1.SelectedIndexChanged
        FillReportBox(CB_Comp1.Text, 0, TextBox1)
    End Sub

    Private Sub CB_Comp2_SelectedIndexChanged(sender As System.Object, e As System.EventArgs) Handles CB_Comp2.SelectedIndexChanged
        FillReportBox(CB_Comp2.Text, 1, TextBox2)
    End Sub

    Public Function IdealVapPressure(ByVal T As Double, ByVal FComp As FSAC_Component) As Double
        Dim A, B, C, D, E, P As Double
        A = CSComponents(FComp.CAS).A
        B = CSComponents(FComp.CAS).B
        C = CSComponents(FComp.CAS).C
        D = CSComponents(FComp.CAS).D
        E = CSComponents(FComp.CAS).E
        P = (Exp(A + B / T + C * Log(T) + D * T ^ E)) 'pressure as [Pa], T as [K]

        Return P

    End Function
    Public Sub Init_FSAC_Calc()
        Dim i As Integer

        'initialise all components
        'this program only supports two component mixtures
        For i = 0 To Comps - 1
            InitFSAC_Comp(FSAC_CompData(i), FSAC_Comps(i))
        Next

        CalcDeltaW()                        'Calculate hydogen bond matrix
        CalcSegGammaPure(TextBox_Temp.Text + 273.15) 'Calculate gamma values of pure components
    End Sub
    Public Sub CalcSegGammaPure(ByVal Temp As Double)
        ' This routine computes the gamma values for the pure compounds in the given temperature.
        ' It must be called after "allocation" and before "activity".
        '
        ' If the system temperature is constant between activity computations, this routine can
        ' be called only once in the program. Every time the temperature changes, this routine
        ' must be called.
        '
        ' Temperature in KELVIN.
        '
        Dim i, j, mm, nn As Integer
        Dim MaxVal, Summation As Double
        Dim SegGammaOldPR(21, 1), ConPR(21, 1) As Double 'local matrix only for itertion

        For i = 0 To Comps - 1 'do for all components

            'init SegGammaPR
            For mm = 1 To 21
                SegGammaPR(mm, i) = 1
            Next

            Do 'iteration of SegGammaPr
                'save oldSegGammaPR
                For mm = 1 To 21
                    SegGammaOldPR(mm, i) = SegGammaPR(mm, i)
                Next

                For mm = 1 To 21
                    Summation = 0
                    For nn = 1 To 21
                        Summation += FSAC_CompData(i).SigmaComp(nn, 2) / FSAC_CompData(i).Q * SegGammaOldPR(nn, i) * Exp(-DeltaW(i, i, mm, nn) / (FSAC_RGAS * Temp))
                    Next
                    SegGammaPR(mm, i) = Exp(-Log(Summation))
                    SegGammaPR(mm, i) = (SegGammaPR(mm, i) + SegGammaOldPR(mm, i)) / 2
                    ConPR(mm, i) = Abs((SegGammaPR(mm, i) - SegGammaOldPR(mm, i)) / SegGammaOldPR(mm, i))
                Next
                MaxVal = 0
                ' calc max deviation
                For j = 0 To 21
                    If ConPR(j, i) > MaxVal Then MaxVal = ConPR(j, i)
                Next
            Loop Until MaxVal <= 0.000001
        Next
    End Sub
    Public Sub CalcDeltaW()
        Dim i, j, mm, nn As Integer

        'Clear Report box
        TextBox3.Clear()

        'init matrix
        For i = 0 To Comps - 1
            For j = 0 To Comps - 1
                For mm = 0 To 21
                    For nn = 0 To 21
                        DeltaW(i, j, mm, nn) = 0
                        DeltaW_HB(i, j, mm, nn) = 0
                    Next
                Next
            Next
        Next

        'calculate HB matrix
        For i = 0 To Comps - 1
            For j = 0 To Comps - 1
                For mm = 0 To 21
                    For nn = 0 To 21
                        'calc matrix
                        If FSAC_CompData(i).SigmaComp(mm, 3) = 0 Or _
                           FSAC_CompData(j).SigmaComp(nn, 3) = 0 Or _
                           FSAC_CompData(i).SigmaComp(mm, 3) = FSAC_CompData(j).SigmaComp(nn, 3) Then

                            DeltaW(i, j, mm, nn) = FSAC_ALPHAPRIME / 2 * FSAC_CompData(i).SigmaComp(mm, 1) + FSAC_CompData(j).SigmaComp(nn, 1) ^ 2
                        Else
                            'Missing HB interaction
                            TextBox3.AppendText("Missing HB interation")
                        End If
                    Next
                Next
            Next
        Next
    End Sub
    Public Sub CalcSegGammaMixture(ByVal T As Double)
        'Routine from Activity.f90
        'This routine computes the segment gamma values for the compounds in mixture
        'T = Temperature [K]

        Dim i, j, mm, nn, L As Integer
        Dim MaxVal, Summation As Double
        
        Dim Converg(21, 1) As Double

        'init calculation
        L = 0
        For j = 0 To 1
            For mm = 1 To 21
                SegGamma(mm, j) = 1
                SegGammaOld(mm, j) = 1
            Next
        Next

        'iteration to calculate Segment-Gammas
        Do
            SegGammaOld = SegGamma
            For i = 0 To Comps - 1 'run through all components
                For mm = 1 To 21
                    Summation = 0
                    For j = 0 To Comps - 1 'run through all component
                        For nn = 1 To 21
                            Summation += ProfileMatrix(nn, j) * SegGammaOld(nn, j) * Exp(-DeltaW(i, j, mm, nn) / FSAC_RGAS / T)
                        Next
                    Next
                    SegGamma(mm, i) = 1 / Summation
                    SegGamma(mm, i) = (SegGamma(mm, i) + SegGammaOld(mm, i)) / 2
                Next
            Next

            L += 1 'count iteration runs

            'calculate maximum convergence error
            MaxVal = 0
            For i = 0 To Comps - 1
                For mm = 1 To 21
                    If Converg(mm, i) > MaxVal Then MaxVal = Converg(mm, i)
                Next
            Next
        Loop Until MaxVal <= 0.000001
    End Sub
    Public Sub InitFSAC_Comp(ByRef FCD As FSAC_CompData, ByVal FC As FSAC_Component)
        'initilise FSAC calculations for component
        'Implementation of subroutines contained in "allocation.f90" and "CompsData.f90"
        '
        'Calculation of Component parameters':
        '       * Sigma profile
        '       * Volume and Surface
        '       * Hydrogen bond donor and acceptor sites

        Dim i, k, GID, mm As Integer 'auxilary variables
        Dim positive_pos, negative_pos, zero_pos As Integer 'finds the positive, zero and negative positions in sigma profile
        Dim Q_zero, sigma_pos, sigma_neg As Double
        Dim SigmaProfile(51, 4) As Double

        Dim initial As Double = -0.025
        Dim inc As Double = 0.001

        '=======================================================
        '=================== subroutines in CompsData.f90 ======
        '=======================================================

        '========================================
        '= subroutine Comps - CompsData.f90 =====
        '========================================
        zero_pos = 26 ' zero position - always element 26
        For i = 0 To FC.Count - 1 'all subgroups of component

            '=====================================================
            '= subroutine Subgroups / Groups - CompsData.f90 =====
            '=====================================================
            GID = FSubGroups(FC.SG(i)).GID 'calc main group ID of actual subgoup
            Q_zero = FSubGroups(FC.SG(i)).Qk - FGroups(GID).Qplus - FGroups(GID).Qminus

            sigma_pos = FGroups(GID).Sigma_pos

            'computes the negativ charge density - if the positive charge density is 0 the negative is also
            If FGroups(GID).Qplus = 0 Then 'Original fortran code is probably wrong! Original: "FGroups(GID).Qminus=0"
                sigma_neg = 0
            Else
                sigma_neg = -sigma_pos * FGroups(GID).Qplus / FGroups(GID).Qminus
            End If

            '========================================
            '= subroutine Comps - CompsData.f90 =====
            '========================================
            positive_pos = (sigma_pos - initial) / inc
            negative_pos = (sigma_neg - initial) / inc

            FCD.SigmaProfile(zero_pos, 2) += FC.GC(i) * Q_zero
            FCD.SigmaProfile(positive_pos + 1, 2) += FC.GC(i) * FGroups(GID).Qplus
            FCD.SigmaProfile(negative_pos + 1, 2) += FC.GC(i) * FGroups(GID).Qminus

            FCD.SigmaProfile(zero_pos, 1) = 0
            FCD.SigmaProfile(positive_pos + 1, 1) = sigma_pos
            FCD.SigmaProfile(negative_pos + 1, 1) = sigma_neg

            FCD.SigmaProfile(zero_pos, 3) = 0
            FCD.SigmaProfile(positive_pos + 1, 3) = FGroups(GID).HBAcc
            FCD.SigmaProfile(negative_pos + 1, 3) = FGroups(GID).HBDon

            FCD.SigmaProfile(zero_pos, 4) = GID
            FCD.SigmaProfile(positive_pos + 1, 4) = GID
            FCD.SigmaProfile(negative_pos + 1, 4) = GID

            FCD.Q += FC.GC(i) * FSubGroups(FC.SG(i)).Qk
            FCD.R += FC.GC(i) * FSubGroups(FC.SG(i)).Rk
        Next

        '=============================================
        '====== module:      Allocation.f90 ==========
        '====== subroutine:  SetSigmaComp ============
        '=============================================
        '== This routine sets the SigmaComp matrix ===
        '=============================================
        mm = 0
        For k = 1 To FSAC_COMPSEG
            If FCD.SigmaProfile(k, 2) <> 0 Then
                mm += 1
                If FCD.SigmaProfile(k, 3) = 0 Then
                    FCD.SigmaComp(mm, 1) = FCD.SigmaProfile(k, 1)
                    FCD.SigmaComp(mm, 2) = FCD.SigmaProfile(k, 2)
                    FCD.SigmaComp(mm, 3) = 0
                    FCD.SigmaComp(mm, 4) = FCD.SigmaProfile(k, 4)
                Else
                    FCD.SigmaComp(mm, 1) = FCD.SigmaProfile(k, 1)
                    FCD.SigmaComp(mm, 2) = FCD.SigmaProfile(k, 2) - FSAC_AEFPRIME * FCD.SigmaProfile(k, 3)
                    FCD.SigmaComp(mm, 3) = 0
                    FCD.SigmaComp(mm, 4) = FCD.SigmaProfile(k, 4)
                    mm += 1
                    FCD.SigmaComp(mm, 1) = FCD.SigmaProfile(k, 1)
                    FCD.SigmaComp(mm, 2) = FSAC_AEFPRIME * FCD.SigmaProfile(k, 3)
                    FCD.SigmaComp(mm, 4) = FCD.SigmaProfile(k, 4)
                    If FCD.SigmaProfile(mm, 1) < 0 Then
                        FCD.SigmaComp(mm, 3) = 1
                    Else
                        FCD.SigmaComp(mm, 3) = 2
                    End If
                End If
            End If
        Next
    End Sub
    Public Function Gamma(ByVal i As Integer, ByVal T As Double, ByVal Xi() As Double) As Double
        'i: index of component where gamma needs to be calculated
        'T: Temperature [K]
        'Comps(): Component ID's of mixture
        'Xi(): mole fractions of components in liquid phase

        Dim LnGComb, LnGRes As Double ' combinatorial and residual part of gamma
        Dim q, r, rs As Double
        Dim Denom As Double
        Dim Fi, Vi, Vis As New ArrayList
        Dim j, k, mm As Integer

        'qi = total surface area of molecule i [Angström²]
        'ri = total volume of molecule i [Angström³]
        'Fi = surface area fraction of molecule i in mixture
        'Vi = volume fraction of molecule i in mixture

        '=============================================================================================================
        '===== combinatorial term - FSAC =============================================================================
        '=============================================================================================================
        'calculate volume and area fractions of components in mixture
        'This code is an implementation of formulas 2 - 6 in first paper
        'Subroutine "CalcStavermanGigenheim" in Fortran code works a little different -> normalisation of R by 66.69 !?

        Fi.Clear()
        Vi.Clear()
        Vis.Clear()
        r = 0
        rs = 0
        q = 0
        For k = 0 To Comps - 1
            r += Xi(k) * FSAC_CompData(k).R '-> BOTV
            q += Xi(k) * FSAC_CompData(k).Q '-> BOTF
            rs += Xi(k) * FSAC_CompData(k).R ^ (3 / 4)
        Next
        For k = 0 To Comps - 1
            Fi.Add(FSAC_CompData(k).Q / q)
            Vi.Add(FSAC_CompData(k).R / r)
            Vis.Add(FSAC_CompData(k).R ^ (3 / 4) / rs)
        Next

        LnGComb = 1 - Vis(i) + Log(Vis(i)) - 5 * FSAC_CompData(i).Q / 50 * (1 - Vi(i) / Fi(i) + Log(Vi(i) / Fi(i)))

        '=============================================================================================================
        '===== residual term - FSAC ==================================================================================
        '=============================================================================================================
        '===== routines from Activity.f90 to calculate activity ====
        '===========================================================

        'Calculate the mixture activity profile
        Denom = 0
        For k = 0 To Comps - 1 'all components
            Denom += Xi(k) * FSAC_CompData(k).Q
        Next
        For j = 0 To Comps - 1
            For mm = 1 To 21
                ProfileMatrix(mm, j) = Xi(j) * FSAC_CompData(j).SigmaComp(mm, 2) / Denom
            Next
        Next

        CalcSegGammaMixture(T)

        LnGRes = 0
        For mm = 1 To 21
            LnGRes += FSAC_CompData(i).SigmaComp(mm, 2) / FSAC_AEFPRIME * Log(SegGamma(mm, i) / SegGammaPR(mm, i))
        Next


        '=============================================================================================================
        '===== calculate gamma of mixture ============================================================================
        '=============================================================================================================
        Gamma = Exp(LnGRes + LnGComb)
    End Function

    Private Sub ButDrawVLEDiagr_Click(sender As System.Object, e As System.EventArgs) Handles ButDrawDiagr.Click
        Dim Xi(2), Yi(2), Pi(2), Ptotal As Double
        Dim xn, xo, dx, Pn, Temp As Double
        Dim Vx, Vy1, Vy2, Vy3, Vy4 As New ArrayList

        'proceed only if components are selected
        If FSAC_Comps(0) Is Nothing Or FSAC_Comps(1) Is Nothing Then
            TextBox3.Clear()
            TextBox3.AppendText("Not all components defined !")
            Exit Sub
        End If

        'initialise F-SAC calculations
        Init_FSAC_Calc()

        'init diagram data tables
        Vx.Clear()
        Vy1.Clear()
        Vy2.Clear()

        Temp = TextBox_Temp.Text + 273.15 'fetch temperature [K]

        'calculate diagram data
        For N = 0 To 50
            Xi(0) = N * 0.02
            Xi(1) = 1 - N * 0.02
            Vx.Add(100 * Xi(0))

            'bubble point - ideal mixture
            Ptotal = Xi(0) * IdealVapPressure(Temp, FSAC_Comps(0)) + Xi(1) * IdealVapPressure(Temp, FSAC_Comps(1))
            Vy1.Add(Ptotal)

            'dew point calculation - ideal mixture
            dx = 0.01
            xo = Xi(0)
            If xo = 0 Or xo = 1 Then
                'Ptotal = Xi(0) * IdealVapPressure(Temp, FSAC_Comps(0)) + Xi(1) * IdealVapPressure(Temp, FSAC_Comps(1))
                Vy2.Add(Ptotal) 'pure component -> Y= total pressure
            Else
                'find direction of search
                Pi(0) = IdealVapPressure(Temp, FSAC_Comps(0))
                Pi(1) = IdealVapPressure(Temp, FSAC_Comps(1))
                If Pi(1) < Pi(0) Then dx = -dx 'if smaller then search into left direction
                Do
                    xn = xo + dx

                    Pi(0) = xn * IdealVapPressure(Temp, FSAC_Comps(0))
                    Pi(1) = (1 - xn) * IdealVapPressure(Temp, FSAC_Comps(1))
                    Pn = Pi(0) + Pi(1)
                    Yi(0) = Pi(0) / Pn
                    Yi(1) = Pi(1) / Pn

                    If (xn - Xi(0)) / (Yi(0) - Xi(0)) > 0 Then
                        dx = dx / 2 'both points on same side: reduce step size
                    Else
                        xo = xn 'both points still on different sides, keep step size of searching
                    End If
                    If (xo + dx <= 0) Or (xo + dx >= 1) Then dx = dx / 2

                Loop Until Abs(dx) <= 0.00001
                Vy2.Add(Pn)
            End If


            'bubble point - real mixture
            Ptotal = 0
            For i = 0 To Comps - 1
                Ptotal += Xi(i) * Gamma(i, Temp, Xi) * IdealVapPressure(Temp, FSAC_Comps(i))
            Next
            Vy3.Add(Ptotal)
        Next


        'setup diagram
        With GraphVLE.GraphPane()
            .CurveList.Clear()
            .Title.Text = "VLE" & vbCrLf & FSAC_Comps(0).Name & "(1) / " & FSAC_Comps(1).Name & "(2)"
            .XAxis.Title.Text = FSAC_Comps(0).Name & " [mol%] "
            .YAxis.Title.Text = "P [Pa]"
            .XAxis.Scale.Max = 100

            With .AddCurve("Boiling ideal", Vx.ToArray(GetType(Double)), Vy1.ToArray(GetType(Double)), Color.Blue, ZedGraph.SymbolType.None)
                .Color = Color.SteelBlue
                .Line.IsSmooth = False
            End With

            With .AddCurve("Dew ideal", Vx.ToArray(GetType(Double)), Vy2.ToArray(GetType(Double)), Color.Maroon, ZedGraph.SymbolType.None)
                .Color = Color.Maroon
                .Line.IsSmooth = False
            End With

            With .AddCurve("Boiling F-SAC", Vx.ToArray(GetType(Double)), Vy1.ToArray(GetType(Double)), Color.Blue, ZedGraph.SymbolType.Circle)
                .Color = Color.SteelBlue
                .Line.IsSmooth = False
                .Symbol.Fill.Type = ZedGraph.FillType.Solid
            End With
        End With
        GraphVLE.AxisChange()
        GraphVLE.Invalidate()
    End Sub

    Private Sub ButDrawGammaDiagr_Click(sender As System.Object, e As System.EventArgs) Handles Button1.Click
        Dim Xi(2), Yi(2), Pi(2) As Double
        Dim Vx, Vy1, Vy2, Vy3, Vy4 As New ArrayList

        'proceed only if both components are selected
        If FSAC_Comps(0) Is Nothing Or FSAC_Comps(1) Is Nothing Then
            TextBox3.Clear()
            TextBox3.AppendText("Not all components defined !")
            Exit Sub
        End If

        'initialise F-SAC calculations
        Init_FSAC_Calc()

        Vx.Clear()
        Vy1.Clear()
        Vy2.Clear()

        For N = 0 To 50
            Xi(0) = N * 0.02
            Xi(1) = 1 - N * 0.02

            Vx.Add(100 * Xi(0))
            Vy1.Add(Gamma(0, 298, Xi))
            Vy2.Add(Gamma(1, 298, Xi))
        Next

        'setup diagram
        With GraphVLE.GraphPane()
            .CurveList.Clear()
            .Title.Text = "Gamma" & vbCrLf & FSAC_Comps(0).Name & "(1) / " & FSAC_Comps(1).Name & "(2)"
            .XAxis.Title.Text = FSAC_Comps(0).Name & " [mol%] "
            .YAxis.Title.Text = "Gamma"
            .XAxis.Scale.Max = 100

            With .AddCurve("Gamma" & FSAC_Comps(0).Name, Vx.ToArray(GetType(Double)), Vy1.ToArray(GetType(Double)), Color.Blue, ZedGraph.SymbolType.None)
                .Color = Color.SteelBlue
                .Line.IsSmooth = False
            End With

            With .AddCurve("Gamma" & FSAC_Comps(0).Name, Vx.ToArray(GetType(Double)), Vy2.ToArray(GetType(Double)), Color.Maroon, ZedGraph.SymbolType.None)
                .Color = Color.Maroon
                .Line.IsSmooth = False
            End With

        End With
        GraphVLE.AxisChange()
        GraphVLE.Invalidate()
    End Sub
End Class

Public Class FSAC_Group
    Private m_GID As Integer
    Private m_Group As String
    Private m_Charge, m_Qplus, m_Qminus, m_sigma_pos, m_HBAcc, m_HBDon As Double

    Public Property GID() As Integer 'Group ID
        Get
            Return m_GID
        End Get
        Set(ByVal value As Integer)
            m_GID = value
        End Set
    End Property
    Public Property Group() As String 'Group
        Get
            Return m_Group
        End Get
        Set(ByVal value As String)
            m_Group = value
        End Set
    End Property
    Public Property Charge() As Double 'Charge
        Get
            Return m_Charge
        End Get
        Set(ByVal value As Double)
            m_Charge = value
        End Set
    End Property
    Public Property Qplus() As Double 'positive area segment of group 
        Get
            Return m_Qplus
        End Get
        Set(ByVal value As Double)
            m_Qplus = value
        End Set
    End Property
    Public Property Qminus() As Double 'negative area segment of group 
        Get
            Return m_Qminus
        End Get
        Set(ByVal value As Double)
            m_Qminus = value
        End Set
    End Property
    Public Property Sigma_pos() As Double 'Sigma positive of group 
        Get
            Return m_sigma_pos
        End Get
        Set(ByVal value As Double)
            m_sigma_pos = value
        End Set
    End Property
    Public Property HBAcc() As Double 'Number of HB acceptor sites of group 
        Get
            Return m_HBAcc
        End Get
        Set(ByVal value As Double)
            m_HBAcc = value
        End Set
    End Property
    Public Property HBDon() As Double 'Number of HB donor sites of group 
        Get
            Return m_HBDon
        End Get
        Set(ByVal value As Double)
            m_HBDon = value
        End Set
    End Property
End Class

Public Class FSAC_Subgroup
    Private m_GID, m_SGID As Integer
    Private m_Group, m_Subgoup As String
    Private m_MW, m_Qk, m_Rk As Double

    Public Property GID() As Integer 'Group ID
        Get
            Return m_GID
        End Get
        Set(ByVal value As Integer)
            m_GID = value
        End Set
    End Property
    Public Property SGID() As Integer 'SubgroupID
        Get
            Return m_SGID
        End Get
        Set(ByVal value As Integer)
            m_SGID = value
        End Set
    End Property
    Public Property Group() As String 'Group
        Get
            Return m_Group
        End Get
        Set(ByVal value As String)
            m_Group = value
        End Set
    End Property
    Public Property Subgroup() As String 'Subgroup
        Get
            Return m_Subgoup
        End Get
        Set(ByVal value As String)
            m_Subgoup = value
        End Set
    End Property
    Public Property MW() As Double 'molecular weight
        Get
            Return m_MW
        End Get
        Set(ByVal value As Double)
            m_MW = value
        End Set
    End Property
    Public Property Qk() As Double 'group surface
        Get
            Return m_Qk
        End Get
        Set(ByVal value As Double)
            m_Qk = value
        End Set
    End Property
    Public Property Rk() As Double 'group volume
        Get
            Return m_Rk
        End Get
        Set(ByVal value As Double)
            m_Rk = value
        End Set
    End Property
End Class
Public Class FSAC_CompData
    'FSAC data of pure component
    Private m_Q, m_R As Double
    Private m_SigmaProfile(51, 4), m_SigmaComp(21, 4) As Double
    Public Property Q() As Double 'molecular surface - unit: A²
        Get
            Return m_Q
        End Get
        Set(ByVal value As Double)
            m_Q = value
        End Set
    End Property
    Public Property R() As Double 'molecular volume - unit A³
        Get
            Return m_R
        End Get
        Set(ByVal value As Double)
            m_R = value
        End Set
    End Property
    Public Property SigmaProfile(ByVal i As Integer, ByVal j As Integer) As Double
        Get
            Return m_SigmaProfile(i, j)
        End Get
        Set(ByVal value As Double)
            m_SigmaProfile(i, j) = value
        End Set
    End Property
    Public Property SigmaComp(ByVal i As Integer, ByVal j As Integer) As Double
        Get
            Return m_SigmaComp(i, j)
        End Get
        Set(ByVal value As Double)
            m_SigmaComp(i, j) = value
        End Set
    End Property
End Class

Public Class FSAC_Component
    'ID;Name;CAS;formula;MW;Fixed;Count;SubGroup1;nu1;SubGroup2;nu2;SubGroup3;nu3;SubGroup4;nu4;;
    Private m_ID, m_Count As Integer
    Private m_Name, m_CAS, m_Formula As String
    Private m_MW As Double
    Private n_gc(3), n_sg(3) As Integer
    Public Property ID() As Integer 'Component ID
        Get
            Return m_ID
        End Get
        Set(ByVal value As Integer)
            m_ID = value
        End Set
    End Property
    Public Property Count() As Integer 'Component subgroup count
        Get
            Return m_Count
        End Get
        Set(ByVal value As Integer)
            m_Count = value
        End Set
    End Property
    Public Property Name() As String 'Name
        Get
            Return m_Name
        End Get
        Set(ByVal value As String)
            m_Name = value
        End Set
    End Property
    Public Property CAS() As String 'CAS number
        Get
            Return m_CAS
        End Get
        Set(ByVal value As String)
            m_CAS = value
        End Set
    End Property
    Public Property Formula() As String 'Name
        Get
            Return m_Formula
        End Get
        Set(ByVal value As String)
            m_Formula = value
        End Set
    End Property
    Public Property MW() As Double 'molecular weight
        Get
            Return m_MW
        End Get
        Set(ByVal value As Double)
            m_MW = value
        End Set
    End Property
    Public Property GC(ByVal i As Integer) As Integer
        Get
            Return n_gc(i)
        End Get
        Set(ByVal value As Integer)
            n_gc(i) = value
        End Set
    End Property
    Public Property SG(ByVal i As Integer) As Integer
        Get
            Return n_sg(i)
        End Get
        Set(ByVal value As Integer)
            n_sg(i) = value
        End Set
    End Property
End Class

Public Class ChemSep_Component
    'Component;MW;Formula;CAS;UNIFAC;MODFAC;Formula ID;A;B;C;D;E;T min;T max;
    Private m_Name, m_CAS, m_Formula, m_UNIFAC, m_MODFAC As String
    Private m_MW, m_A, m_B, m_C, m_D, m_E, m_TMin, m_TMax As Double
    Public Property Name() As String 'Component Name
        Get
            Return m_Name
        End Get
        Set(ByVal value As String)
            m_Name = value
        End Set
    End Property
    Public Property CAS() As String 'Component CAS-number
        Get
            Return m_CAS
        End Get
        Set(ByVal value As String)
            m_CAS = value
        End Set
    End Property
    Public Property Formula() As String 'Component Formula
        Get
            Return m_Formula
        End Get
        Set(ByVal value As String)
            m_Formula = value
        End Set
    End Property
    Public Property UNIFAC() As String 'Component UNIFAC groups
        Get
            Return m_UNIFAC
        End Get
        Set(ByVal value As String)
            m_UNIFAC = value
        End Set
    End Property
    Public Property MODFAC() As String 'Component MODFAC groups
        Get
            Return m_MODFAC
        End Get
        Set(ByVal value As String)
            m_MODFAC = value
        End Set
    End Property
    Public Property MW() As Double 'Component Molecular weight
        Get
            Return m_MW
        End Get
        Set(ByVal value As Double)
            m_MW = value
        End Set
    End Property
    Public Property A() As Double 'Component Vapour pressure formula coefficient A
        Get
            Return m_A
        End Get
        Set(ByVal value As Double)
            m_A = value
        End Set
    End Property
    Public Property B() As Double 'Component Vapour pressure formula coefficient B
        Get
            Return m_B
        End Get
        Set(ByVal value As Double)
            m_B = value
        End Set
    End Property
    Public Property C() As Double 'Component Vapour pressure formula coefficient C
        Get
            Return m_C
        End Get
        Set(ByVal value As Double)
            m_C = value
        End Set
    End Property
    Public Property D() As Double 'Component Vapour pressure formula coefficient D
        Get
            Return m_D
        End Get
        Set(ByVal value As Double)
            m_D = value
        End Set
    End Property
    Public Property E() As Double 'Component Vapour pressure formula coefficient E
        Get
            Return m_E
        End Get
        Set(ByVal value As Double)
            m_E = value
        End Set
    End Property
    Public Property TMin() As Double 'Component Vapour pressure formula Minimum Temperature
        Get
            Return m_TMin
        End Get
        Set(ByVal value As Double)
            m_TMin = value
        End Set
    End Property
    Public Property TMax() As Double 'Component Vapour pressure formula Maximum Temperature
        Get
            Return m_TMax
        End Get
        Set(ByVal value As Double)
            m_TMax = value
        End Set
    End Property
End Class