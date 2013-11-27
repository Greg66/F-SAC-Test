<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class Form1
    Inherits System.Windows.Forms.Form

    'Das Formular überschreibt den Löschvorgang, um die Komponentenliste zu bereinigen.
    <System.Diagnostics.DebuggerNonUserCode()> _
    Protected Overrides Sub Dispose(ByVal disposing As Boolean)
        Try
            If disposing AndAlso components IsNot Nothing Then
                components.Dispose()
            End If
        Finally
            MyBase.Dispose(disposing)
        End Try
    End Sub

    'Wird vom Windows Form-Designer benötigt.
    Private components As System.ComponentModel.IContainer

    'Hinweis: Die folgende Prozedur ist für den Windows Form-Designer erforderlich.
    'Das Bearbeiten ist mit dem Windows Form-Designer möglich.  
    'Das Bearbeiten mit dem Code-Editor ist nicht möglich.
    <System.Diagnostics.DebuggerStepThrough()> _
    Private Sub InitializeComponent()
        Me.Label1 = New System.Windows.Forms.Label()
        Me.Label2 = New System.Windows.Forms.Label()
        Me.CB_Comp1 = New System.Windows.Forms.ComboBox()
        Me.CB_Comp2 = New System.Windows.Forms.ComboBox()
        Me.TextBox1 = New System.Windows.Forms.TextBox()
        Me.TextBox2 = New System.Windows.Forms.TextBox()
        Me.GraphVLE = New ZedGraph.ZedGraphControl()
        Me.ButDrawDiagr = New System.Windows.Forms.Button()
        Me.Button1 = New System.Windows.Forms.Button()
        Me.TextBox3 = New System.Windows.Forms.TextBox()
        Me.Label3 = New System.Windows.Forms.Label()
        Me.TextBox_Temp = New System.Windows.Forms.TextBox()
        Me.SuspendLayout()
        '
        'Label1
        '
        Me.Label1.AutoSize = True
        Me.Label1.Location = New System.Drawing.Point(12, 14)
        Me.Label1.Name = "Label1"
        Me.Label1.Size = New System.Drawing.Size(43, 13)
        Me.Label1.TabIndex = 0
        Me.Label1.Text = "Comp 1"
        '
        'Label2
        '
        Me.Label2.AutoSize = True
        Me.Label2.Location = New System.Drawing.Point(383, 14)
        Me.Label2.Name = "Label2"
        Me.Label2.Size = New System.Drawing.Size(43, 13)
        Me.Label2.TabIndex = 1
        Me.Label2.Text = "Comp 2"
        '
        'CB_Comp1
        '
        Me.CB_Comp1.FormattingEnabled = True
        Me.CB_Comp1.Location = New System.Drawing.Point(61, 11)
        Me.CB_Comp1.Name = "CB_Comp1"
        Me.CB_Comp1.Size = New System.Drawing.Size(288, 21)
        Me.CB_Comp1.Sorted = True
        Me.CB_Comp1.TabIndex = 2
        '
        'CB_Comp2
        '
        Me.CB_Comp2.FormattingEnabled = True
        Me.CB_Comp2.Location = New System.Drawing.Point(432, 11)
        Me.CB_Comp2.Name = "CB_Comp2"
        Me.CB_Comp2.Size = New System.Drawing.Size(288, 21)
        Me.CB_Comp2.Sorted = True
        Me.CB_Comp2.TabIndex = 3
        '
        'TextBox1
        '
        Me.TextBox1.Location = New System.Drawing.Point(15, 38)
        Me.TextBox1.Multiline = True
        Me.TextBox1.Name = "TextBox1"
        Me.TextBox1.ScrollBars = System.Windows.Forms.ScrollBars.Both
        Me.TextBox1.Size = New System.Drawing.Size(334, 123)
        Me.TextBox1.TabIndex = 4
        '
        'TextBox2
        '
        Me.TextBox2.Location = New System.Drawing.Point(386, 38)
        Me.TextBox2.Multiline = True
        Me.TextBox2.Name = "TextBox2"
        Me.TextBox2.ScrollBars = System.Windows.Forms.ScrollBars.Both
        Me.TextBox2.Size = New System.Drawing.Size(334, 123)
        Me.TextBox2.TabIndex = 5
        '
        'GraphVLE
        '
        Me.GraphVLE.BorderStyle = System.Windows.Forms.BorderStyle.Fixed3D
        Me.GraphVLE.Font = New System.Drawing.Font("Arial", 8.25!)
        Me.GraphVLE.IsAntiAlias = True
        Me.GraphVLE.IsAutoScrollRange = True
        Me.GraphVLE.IsShowPointValues = True
        Me.GraphVLE.Location = New System.Drawing.Point(15, 266)
        Me.GraphVLE.Name = "GraphVLE"
        Me.GraphVLE.ScrollGrace = 0.0R
        Me.GraphVLE.ScrollMaxX = 0.0R
        Me.GraphVLE.ScrollMaxY = 0.0R
        Me.GraphVLE.ScrollMaxY2 = 0.0R
        Me.GraphVLE.ScrollMinX = 0.0R
        Me.GraphVLE.ScrollMinY = 0.0R
        Me.GraphVLE.ScrollMinY2 = 0.0R
        Me.GraphVLE.Size = New System.Drawing.Size(705, 397)
        Me.GraphVLE.TabIndex = 12
        '
        'ButDrawDiagr
        '
        Me.ButDrawDiagr.Location = New System.Drawing.Point(15, 237)
        Me.ButDrawDiagr.Name = "ButDrawDiagr"
        Me.ButDrawDiagr.Size = New System.Drawing.Size(126, 23)
        Me.ButDrawDiagr.TabIndex = 13
        Me.ButDrawDiagr.Text = "Draw VLE Diagram"
        Me.ButDrawDiagr.UseVisualStyleBackColor = True
        '
        'Button1
        '
        Me.Button1.Location = New System.Drawing.Point(147, 237)
        Me.Button1.Name = "Button1"
        Me.Button1.Size = New System.Drawing.Size(126, 23)
        Me.Button1.TabIndex = 14
        Me.Button1.Text = "Draw Gamma Diagram"
        Me.Button1.UseVisualStyleBackColor = True
        '
        'TextBox3
        '
        Me.TextBox3.Location = New System.Drawing.Point(15, 167)
        Me.TextBox3.Multiline = True
        Me.TextBox3.Name = "TextBox3"
        Me.TextBox3.ScrollBars = System.Windows.Forms.ScrollBars.Both
        Me.TextBox3.Size = New System.Drawing.Size(705, 64)
        Me.TextBox3.TabIndex = 15
        '
        'Label3
        '
        Me.Label3.AutoSize = True
        Me.Label3.Location = New System.Drawing.Point(383, 242)
        Me.Label3.Name = "Label3"
        Me.Label3.Size = New System.Drawing.Size(87, 13)
        Me.Label3.TabIndex = 16
        Me.Label3.Text = "Temperature [°C]"
        '
        'TextBox_Temp
        '
        Me.TextBox_Temp.Location = New System.Drawing.Point(476, 239)
        Me.TextBox_Temp.Name = "TextBox_Temp"
        Me.TextBox_Temp.Size = New System.Drawing.Size(100, 20)
        Me.TextBox_Temp.TabIndex = 17
        Me.TextBox_Temp.Text = "20"
        '
        'Form1
        '
        Me.AutoScaleDimensions = New System.Drawing.SizeF(6.0!, 13.0!)
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.ClientSize = New System.Drawing.Size(732, 675)
        Me.Controls.Add(Me.TextBox_Temp)
        Me.Controls.Add(Me.Label3)
        Me.Controls.Add(Me.TextBox3)
        Me.Controls.Add(Me.Button1)
        Me.Controls.Add(Me.ButDrawDiagr)
        Me.Controls.Add(Me.GraphVLE)
        Me.Controls.Add(Me.TextBox2)
        Me.Controls.Add(Me.TextBox1)
        Me.Controls.Add(Me.CB_Comp2)
        Me.Controls.Add(Me.CB_Comp1)
        Me.Controls.Add(Me.Label2)
        Me.Controls.Add(Me.Label1)
        Me.Name = "Form1"
        Me.StartPosition = System.Windows.Forms.FormStartPosition.CenterScreen
        Me.Text = "F-SAC Test Application"
        Me.ResumeLayout(False)
        Me.PerformLayout()

    End Sub
    Friend WithEvents Label1 As System.Windows.Forms.Label
    Friend WithEvents Label2 As System.Windows.Forms.Label
    Friend WithEvents CB_Comp1 As System.Windows.Forms.ComboBox
    Friend WithEvents CB_Comp2 As System.Windows.Forms.ComboBox
    Friend WithEvents TextBox1 As System.Windows.Forms.TextBox
    Friend WithEvents TextBox2 As System.Windows.Forms.TextBox
    Public WithEvents GraphVLE As ZedGraph.ZedGraphControl
    Friend WithEvents ButDrawDiagr As System.Windows.Forms.Button
    Friend WithEvents Button1 As System.Windows.Forms.Button
    Friend WithEvents TextBox3 As System.Windows.Forms.TextBox
    Friend WithEvents Label3 As System.Windows.Forms.Label
    Friend WithEvents TextBox_Temp As System.Windows.Forms.TextBox

End Class
