


Public Class Form1
    Private cont As Integer
    Private webcam As WebCam
    Private flagstop As Boolean
    Private TimerTick As Boolean
    Public previsaoEspera As Date
    Public TotalissimoB As Integer
    Public MediaTotalissimoB As Integer

    Public PontoTit As Integer
    Private NumPontoTit As Integer
    Private SinalR(10000), SinalG(10000), SinalB(10000) As Integer
    Private caminho As String
    Private ValoresX As New List(Of Integer)
    Public PassodeAngulo As Integer
    'Dim Writer As DigitalSingleChannelWriter
    Dim Tarefa As Task
    Dim Int As Integer
    Dim NumLeituras As Integer
    Dim ContLeituras As Integer
    Dim NumPicos As Integer
    Dim ContPicos As Integer
    Dim Flaginjetar As Boolean
    Dim vazao1 As Integer
    Dim vazao2 As Integer
    Dim comando1 As String
    Dim comando2 As String
    Dim ptsX As New List(Of Double)
    Dim ptsY As New List(Of Double)
    Dim counter As Integer = 1
    Dim conc As Double = 0
    Dim nova_conc As Double = 0







    Function MakeScreenShot() As Drawing.Bitmap
        Dim out As Drawing.Bitmap

        'Get the screen Size
        Dim bounds As Rectangle = Screen.GetBounds(Point.Empty)

        'create the bitmap
        out = New Drawing.Bitmap(bounds.Width, bounds.Height)

        'create a graphic object to recive the pic
        Using gr As Drawing.Graphics = Graphics.FromImage(out)
            'Copy the screen using built in API
            gr.CopyFromScreen(Point.Empty, Point.Empty, bounds.Size)
        End Using

        Return out
    End Function









    Private Sub mainWinForm_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load
        webcam = New WebCam()
        webcam.InitializeWebCam(imgVideo)
        'imgVideo.AllowDrop = True
        SerialPort1.Open()
        LabelAVISO.Text = "INICIAR"


    End Sub

    Public Sub Espera(ByVal duracao As Double, Optional ByVal unidade As String = "ms")
        'If miliseg > 0 Or Not miliseg = 1 / 0 Or Not Double.IsNaN(miliseg) Then
        'MsgBox(miliseg)
        Dim miliseg As Double
        If unidade = "ms" Then
            miliseg = duracao + 100
        ElseIf unidade = "seg" Then
            miliseg = duracao * 1000 + 100
        ElseIf unidade = "min" Then
            miliseg = duracao * 1000 * 60 + 100
        Else
            miliseg = duracao + 100
        End If
        TimerEspera = New Timer
        TimerEspera.Interval = miliseg
        TimerEspera.Enabled = True
        TimerTick = True
        'tempoDeEspera = New TimeSpan(miliseg)
        previsaoEspera = Now.AddMilliseconds(miliseg)
        Do While TimerTick
            Application.DoEvents()
        Loop
        TimerEspera.Dispose()
        'End If

    End Sub

    Public Sub PararEspera()
        If Not IsNothing(TimerEspera) Then
            TimerEspera.Dispose()
            'É importante colocar timertick como false, caso contrário o thread ficara preso no loop Do While da sub Espera 
            TimerTick = False
        End If
    End Sub

    Private Sub EnviaDigitalV(ByVal ValvByte As Integer)
        'Tarefa = New Task
        'Tarefa.DOChannels.CreateChannel("Dev1/port0/line0:7", "", ChannelLineGrouping.OneChannelForAllLines)
        'Writer = New DigitalSingleChannelWriter(Tarefa.Stream)
        'Dim bits As Byte = ValvByte
        ' Writer.WriteSingleSamplePort(True, bits)
    End Sub

    Private Sub RotinaMulticomut()


        'vazao1 = NumericUpDown4.Value
        'vazao2 = NumericUpDown3.Value

        comando1 = CStr(vazao1)
        comando2 = CStr(vazao2)

        If vazao1 < 10 Then
            comando1 = "0" & comando1
        End If

        If vazao2 < 10 Then
            comando2 = "0" & comando2
        End If


        LabelAVISO.Text = "AGUARDE"




        'If CheckTrocaSolucao.Checked = True Then
        'SerialPort1.Write("1S00" & comando1 & vbCrLf)
        'End If


        'If CheckTrocaSolucao.Checked = False Then
        'SerialPort1.Write("1S00" & comando2 & vbCrLf)
        'End If

        TimerTick = True

        Timer3.Interval = 2000
        Timer3.Enabled = True


        While TimerTick = True
            Application.DoEvents()
        End While





        For i = 0 To Val(NumCiclos.Value) - 1


            '################################# Injeta paracetamol#######################################
            Int = 7 '3
            EnviaDigitalV(Int)

            LabelAVISO.Text = "INJETANDO PARACETAMOL"

            TimerTick = True

            Timer3.Interval = Val(IntervalPR.Value) * 1000
            Timer3.Enabled = True


            While TimerTick = True
                Application.DoEvents()
            End While


            '################################# Injeta hipoclorito#######################################

            Int = 7 '5
            EnviaDigitalV(Int)

            LabelAVISO.Text = "INJETANDO HIPOCLORITO"

            ' TimerTick = True

            'Timer3.Interval = Val(IntervalHipo.Value) * 1000
            'Timer3.Enabled = True


            ' While TimerTick = True
            'Application.DoEvents()
            ' End While


        Next



        '################################# Injeta paracetamol#######################################
        'Int = 0 '3
        'EnviaDigitalV(Int)

        ' LabelAVISO.Text = "INJETANDO PARACETAMOL"

        ' TimerTick = True

        'Timer3.Interval = Val(IntervalPR.Value) * 1000
        'Timer3.Enabled = True


        ' While TimerTick = True
        'Application.DoEvents()
        ' End While



        Int = 0
        EnviaDigitalV(Int)



        SerialPort1.Write("1I" & vbCrLf)

        LabelAVISO.Text = "PAUSA BOMBA"

        'TimerTick = True

        ' Timer3.Interval = Val(IntervalBomba.Value) * 1000
        'Timer3.Enabled = True


        ' While TimerTick = True
        'Application.DoEvents()
        'End While

        SerialPort1.Write("1S00" & comando1 & vbCrLf)


        TimerTick = True

        Timer3.Interval = 2000
        Timer3.Enabled = True


        While TimerTick = True
            Application.DoEvents()
        End While


    End Sub


    Private Sub bntStart_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles btnStart.Click
        webcam.Start()
    End Sub

    Private Sub bntStop_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles btnStop.Click
        webcam.Stop()
    End Sub

    'Private Sub bntContinue_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles btnContinue.Click
    'webcam.Continue()
    'End Sub

    Private Sub bntCapture_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles btnCapture.Click
        imgCapture.Image = imgVideo.Image
        With Helper.GetImageAverageRGB(imgCapture.Image)
            tx_R_i.Text = .R
            tx_G_i.Text = .G
            tx_B_i.Text = .B
        End With
    End Sub

    Private Sub bntSave_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles btnSave.Click

        webcam.Start()

        imgCapture.Image = imgVideo.Image

        Helper.DefineCapture(imgCapture.Image)

        'For k = 0 To 3

        Helper.SaveImageCapture(imgCapture.Image)

        TimerTick = True

        Timer3.Interval = 3000
        Timer3.Enabled = True


        While TimerTick = True
            Application.DoEvents()
        End While

        imgCapture.Image = imgVideo.Image

        ' Next





    End Sub

    Private Sub bntVideoFormat_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles btnVideoFormat.Click
        webcam.ResolutionSetting()
    End Sub

    Private Sub bntVideoSource_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles btnVideoSource.Click
        webcam.AdvanceSetting()
    End Sub

    Dim nPoints_Graph As Integer = 100

    Private Sub Timer1_Tick(sender As Object, e As EventArgs) Handles Timer1.Tick


        Dim tempBitmap As Bitmap = New Bitmap(imgVideo.ClientSize.Width, imgVideo.ClientSize.Height)
        imgVideo.DrawToBitmap(tempBitmap, imgVideo.ClientRectangle)


        Dim MeuRetangulo As New RectangleF(PictureBox1.Location, PictureBox1.Size)


        'Copy the selection rectangle from the temporary snapshot to the target bitmap:
        Dim target As New Bitmap(PictureBox1.ClientSize.Width, PictureBox1.ClientSize.Height) 'retangle width and height
        Using g As Graphics = Graphics.FromImage(target)
            g.DrawImage(tempBitmap, target.GetBounds(0), MeuRetangulo, GraphicsUnit.Pixel)
        End Using

        'Clear up: you don't need tempBitmap any more:
        tempBitmap.Dispose()

        'Ready!
        PictureBox1.Image = target



        Dim valR, valG, valB As Integer
        With Helper.GetImageAverageRGB(PictureBox1.Image)
            valR = .R
            valG = .G
            valB = .B
        End With

        tx_R_v.Text = valR
        tx_G_v.Text = valG
        tx_B_v.Text = valB

        Chart1.Series(0).Points.Add(valR)
        Chart1.Series(1).Points.Add(valG)
        Chart1.Series(2).Points.Add(valB)

        Chart1.ChartAreas(0).AxisX.Minimum = IIf(Chart1.Series(0).Points.Count > nPoints_Graph, Chart1.Series(0).Points.Count - nPoints_Graph, 0)
        Chart1.ChartAreas(0).AxisX.Maximum = IIf(Chart1.Series(0).Points.Count > nPoints_Graph, Chart1.Series(0).Points.Count, nPoints_Graph)

        'Verificando limites impostos e soando alarme

        If cb_R.Checked Then
            If valR > nup_R.Value Then
                Timer1.Enabled = False
                MsgBox("Limite de Vermelho ultrapassado.")
            End If
        ElseIf cb_G.Checked Then
            If valG > nup_G.Value Then
                Timer1.Enabled = False
                MsgBox("Limite de Verde ultrapassado.")
            End If
        ElseIf cb_B.Checked Then
            If valB > nup_B.Value Then
                Timer1.Enabled = False
                MsgBox("Limite de Azul ultrapassado.")
            End If
        End If
    End Sub

    Private Sub Button1_Click(sender As Object, e As EventArgs) Handles btn_monit_RGB.Click
        Timer1.Interval = RGBrate.Value
        Timer1.Enabled = Not Timer1.Enabled
    End Sub

    Private Sub btn_clear_Graph_Click(sender As Object, e As EventArgs) Handles btn_clear_Graph.Click
        Chart1.Series(0).Points.Clear()
        Chart1.Series(1).Points.Clear()
        Chart1.Series(2).Points.Clear()
        Chart1.ChartAreas(0).AxisX.Minimum = 0
        Chart1.ChartAreas(0).AxisX.Maximum = nPoints_Graph
    End Sub


    Private Sub RGBrate_ValueChanged(sender As Object, e As EventArgs) Handles RGBrate.ValueChanged
        Timer1.Interval = RGBrate.Value
    End Sub

    Private Sub bomba_Click(sender As Object, e As EventArgs)

        'SerialPort1.Write("/1" & TextBox1.Text & "R" & vbCrLf)


    End Sub

    Private Sub Button1_Click_1(sender As Object, e As EventArgs)


        For i = 0 To 99

            Dim BM As Drawing.Bitmap = MakeScreenShot()
            Dim mouseloc As Point = Cursor.Position
            Dim c As Color = BM.GetPixel(mouseloc.X, mouseloc.Y) ' The Slowest way possable to read a color

            'Debug.Print(c.R & "," & c.G & "," & c.B)

            'TextBox2.Text = (c.R & "," & c.G & "," & c.B)

            'For i = 0 To 100000000000000
            'TextBox3.Text = "oi"
            'Next

            Timer2.Start()
            'Timer2.Stop()
        Next
        'End While

    End Sub

    Private Sub Button2_Click(sender As Object, e As EventArgs)
        Timer2.Start()
    End Sub

    Private Sub Button3_Click(sender As Object, e As EventArgs)
        Timer2.Stop()


        'PictureBox1.Image = imgVideo.Image
    End Sub

    Private Sub Timer2_Tick(sender As Object, e As EventArgs) Handles Timer2.Tick

        Dim valR, valG, valB As Integer 'alan


        Dim BM As Drawing.Bitmap = MakeScreenShot()
        Dim mouseloc As Point = Cursor.Position
        Dim c As Color = BM.GetPixel(mouseloc.X, mouseloc.Y) ' The Slowest way possable to read a color

        'Debug.Print(c.R & "," & c.G & "," & c.B)

        'TextBox2.Text = (c.R & "," & c.G & "," & c.B)



        'With Helper.GetImageAverageRGB(imgVideo.Image)
        valR = c.R
        valG = c.G
        valB = c.B
        'End With

        tx_R_v.Text = valR
        tx_G_v.Text = valG
        tx_B_v.Text = valB

        Chart1.Series(0).Points.Add(valR)
        Chart1.Series(1).Points.Add(valG)
        Chart1.Series(2).Points.Add(valB)

        Chart1.ChartAreas(0).AxisX.Minimum = IIf(Chart1.Series(0).Points.Count > nPoints_Graph, Chart1.Series(0).Points.Count - nPoints_Graph, 0)
        Chart1.ChartAreas(0).AxisX.Maximum = IIf(Chart1.Series(0).Points.Count > nPoints_Graph, Chart1.Series(0).Points.Count, nPoints_Graph)

        'Verificando limites impostos e soando alarme

        If cb_R.Checked Then
            If valR > nup_R.Value Then
                Timer1.Enabled = False
                MsgBox("Limite de Vermelho ultrapassado.")
            End If
        ElseIf cb_G.Checked Then
            If valG > nup_G.Value Then
                Timer1.Enabled = False
                MsgBox("Limite de Verde ultrapassado.")
            End If
        ElseIf cb_B.Checked Then
            If valB > nup_B.Value Then
                Timer1.Enabled = False
                MsgBox("Limite de Azul ultrapassado.")
            End If
        End If

    End Sub

    Private Sub Button4_Click(sender As Object, e As EventArgs)

        'PictureBox1.Container

        'Take a temporary snapshot of the picture box:
        Dim tempBitmap As Bitmap = New Bitmap(imgVideo.ClientSize.Width, imgVideo.ClientSize.Height)
        imgVideo.DrawToBitmap(tempBitmap, imgVideo.ClientRectangle)





        Dim MeuRetangulo As New RectangleF(PictureBox1.Location, PictureBox1.Size)



        'Copy the selection rectangle from the temporary snapshot to the target bitmap:
        Dim target As New Bitmap(PictureBox1.ClientSize.Width, PictureBox1.ClientSize.Height) 'retangle width and height
        Using g As Graphics = Graphics.FromImage(target)
            g.DrawImage(tempBitmap, target.GetBounds(0), MeuRetangulo, GraphicsUnit.Pixel)
        End Using

        'Clear up: you don't need tempBitmap any more:
        tempBitmap.Dispose()

        'Ready!
        PictureBox1.Image = target

    End Sub



    'Private Sub picturebox1_MouseDown(ByVal sender As Object, ByVal e As System.Windows.Forms.MouseEventArgs) Handles PictureBox1.MouseDown

    'Dim point As New Point
    'point = Cursor.Position


    'PictureBox1.Location = point


    'End Sub



    Private Sub NumericUpDown1_ValueChanged(sender As Object, e As EventArgs) Handles NumericUpDown1.ValueChanged

        Dim j As Point = New Point(NumericUpDown1.Value, NumericUpDown2.Value)


            PictureBox1.Size = j


    End Sub

    Private Sub NumericUpDown2_ValueChanged(sender As Object, e As EventArgs) Handles NumericUpDown2.ValueChanged

        Dim j As Point = New Point(NumericUpDown1.Value, NumericUpDown2.Value)


            PictureBox1.Size = j


    End Sub

    Private Sub imgVideo_Click(sender As Object, e As EventArgs) Handles imgVideo.Click

        'Dim z As Point = New Point(Cursor.Position)
        'Dim d As Point = New Point(0, -20)


        'PictureBox1.Location = z + d



    End Sub



    Private Sub PictureBox1_Click(sender As Object, e As EventArgs) Handles PictureBox1.Click

    End Sub

    Private Sub Label12_Click(sender As Object, e As EventArgs) Handles Label12.Click

    End Sub

    Private Sub Label13_Click(sender As Object, e As EventArgs) Handles Label13.Click

    End Sub

    Private Sub NumericUpDown4_ValueChanged(sender As Object, e As EventArgs)

    End Sub

    Private Sub Label16_Click(sender As Object, e As EventArgs)

    End Sub

    Private Sub Button1_Click_2(sender As Object, e As EventArgs) Handles Button1.Click

        'Dim passos As Double

        'passos = (3000 * Val(NumericUpDown4.Value)) / 5000

        Chart1.Series(0).Points.Clear()
        Chart1.Series(1).Points.Clear()
        Chart1.Series(2).Points.Clear()


        Chart2.ChartAreas(0).AxisX.Minimum = 0 'IIf(Chart1.Series(0).Points.Count > nPoints_Graph, Chart1.Series(0).Points.Count - nPoints_Graph, 0)
        Chart2.ChartAreas(0).AxisX.Maximum = 90 'IIf(Chart1.Series(0).Points.Count > nPoints_Graph, Chart1.Series(0).Points.Count, nPoints_Graph)


        Chart2.Series(0).Points.Clear()

        Array.Clear(SinalR, 0, 10000)
        Array.Clear(SinalG, 0, 10000)
        Array.Clear(SinalB, 0, 10000)

        flagstop = False

        conc = 0

        NumPontoTit = Val(NumXmax.Value)

        Chart1.ChartAreas(0).AxisX.Maximum = NumPontoTit
        Chart1.ChartAreas(0).AxisX.Minimum = 0

        Chart1.ChartAreas(0).AxisY.Maximum = NumericUpDown7.Value 'ESCALA Y
        Chart1.ChartAreas(0).AxisY.Minimum = 0

        webcam.Start()
        PontoTit = 0

        'MsgBox("Escolha um nome e local para salvar as fotos")
        'imgCapture.Image = imgVideo.Image

        'Helper.DefineCapture(imgCapture.Image)

        MsgBox("Escolha um nome e local para salvar os dados")
        SaveFileDialog1.FileName = "dados"
        'Dim ArquivoDados As String = SaveFileDialog1.FileName

        SaveFileDialog1.ShowDialog()
        caminho = SaveFileDialog1.FileName + ".txt"
        'My.Computer.FileSystem.DeleteFile(caminho)



        While flagstop = False

            Flaginjetar = False

            Console.Beep(2000, 500)

            While Flaginjetar = False
                Application.DoEvents()
                LabelAVISO.Text = "DISPARAR"
            End While

            Dim Spots As Integer
            Spots = NumCiclos.Value
            Dim a As Integer

            For a = 0 To Spots - 1

                LabelAVISO.Text = "AGUARDE"
                PassodeAngulo = 15 / 0.1758
                SerialPort1.Write(CStr(PassodeAngulo))


                TimerTick = True

                Timer3.Interval = 6000 'INTERVALO ENTRE LEITURAS MS
                Timer3.Enabled = True


                While TimerTick = True
                    Application.DoEvents()
                End While









                NumPicos = 1 'NumNumPicos.Value



                For p = 0 To NumPicos - 1


                    'RotinaMulticomut()

                    'SerialPort1.Write("1H" & vbCrLf)



                    NumLeituras = NumNumLeituras.Value
                    ContLeituras = 0

                    While ContLeituras < NumLeituras

                        LabelAVISO.Text = "REALIZANDO LEITURAS"
                        ContLeituras = ContLeituras + 1



                        TimerTick = True

                        Timer3.Interval = RGBrate.Value 'INTERVALO ENTRE LEITURAS MS
                        Timer3.Enabled = True


                        While TimerTick = True
                            Application.DoEvents()
                        End While


                        ValoresX.Add(PontoTit)

                        Application.DoEvents()

                        If flagstop = True Then
                            Exit Sub
                        End If


                        imgCapture.Image = imgVideo.Image

                        ' Helper.SaveImageCapture(imgCapture.Image)



                        Dim tempBitmap1 As Bitmap = New Bitmap(imgVideo.ClientSize.Width, imgVideo.ClientSize.Height)
                        imgVideo.DrawToBitmap(tempBitmap1, imgVideo.ClientRectangle)

                        Dim tempBitmap2 As Bitmap = New Bitmap(imgVideo.ClientSize.Width, imgVideo.ClientSize.Height)
                        imgVideo.DrawToBitmap(tempBitmap2, imgVideo.ClientRectangle)

                        Dim tempBitmap3 As Bitmap = New Bitmap(imgVideo.ClientSize.Width, imgVideo.ClientSize.Height)
                        imgVideo.DrawToBitmap(tempBitmap3, imgVideo.ClientRectangle)


                        Application.DoEvents()
                        Dim MeuRetangulo1 As New RectangleF(PictureBox1.Location, PictureBox1.Size)
                        'Dim MeuRetangulo2 As New RectangleF(PictureBox2.Location, PictureBox2.Size)
                        'Dim MeuRetangulo3 As New RectangleF(PictureBox3.Location, PictureBox3.Size)
                        Dim ValR1, ValR2, ValR3 As Integer
                        Dim ValG1, ValG2, ValG3 As Integer
                        Dim ValB1, ValB2, ValB3 As Integer

                        Dim valR, valG, valB As Integer
                        Dim SMvalR, SMvalG, SMvalB As Integer
                        Dim MDvalR, MDvalG, MDvalB As Integer
                        'Dim target As New Bitmap(PictureBox1.ClientSize.Width, PictureBox1.ClientSize.Height) 'retangle width and height



                        valR = 0
                        valG = 0
                        valB = 0

                        SMvalR = 0
                        SMvalG = 0
                        SMvalB = 0

                        MDvalR = 0
                        MDvalG = 0
                        MDvalB = 0

                        'Dim repliIMG As Integer
                        'repliIMG = Val(NumericUpDown5)


                        For t = 0 To NumericUpDown5.Value - 1



                            '#######################################################################################
                            'Copy the selection rectangle from the temporary snapshot to the target bitmap:
                            Dim target1 As New Bitmap(PictureBox1.ClientSize.Width, PictureBox1.ClientSize.Height) 'retangle width and height
                            Using g As Graphics = Graphics.FromImage(target1)
                                g.DrawImage(tempBitmap1, target1.GetBounds(0), MeuRetangulo1, GraphicsUnit.Pixel)
                            End Using

                            'Clear up: you don't need tempBitmap any more:
                            'tempBitmap.Dispose()

                            'Ready!
                            PictureBox1.Image = target1
                            '#######################################################################################


                            'Dim target2 As New Bitmap(PictureBox2.ClientSize.Width, PictureBox2.ClientSize.Height) 'retangle width and height
                            'Using g As Graphics = Graphics.FromImage(target2)
                            'g.DrawImage(tempBitmap2, target2.GetBounds(0), MeuRetangulo2, GraphicsUnit.Pixel)
                            'End Using



                            'Ready!
                            'PictureBox2.Image = target2
                            '#######################################################################################



                            'Dim target3 As New Bitmap(PictureBox3.ClientSize.Width, PictureBox3.ClientSize.Height) 'retangle width and height
                            'Using g As Graphics = Graphics.FromImage(target3)
                            'g.DrawImage(tempBitmap3, target3.GetBounds(0), MeuRetangulo3, GraphicsUnit.Pixel)
                            'End Using



                            'Ready!
                            'PictureBox3.Image = target3



                            'Dim valR, valG, valB As Integer

                            'For t = 0 to 10

                            With Helper.GetImageAverageRGB(PictureBox1.Image)
                                ValR1 = .R
                                ValG1 = .G
                                ValB1 = .B
                            End With

                            'With Helper.GetImageAverageRGB(PictureBox2.Image)
                            'ValR2 = .R
                            'ValG2 = .G
                            'ValB2 = .B
                            'End With


                            'With Helper.GetImageAverageRGB(PictureBox3.Image)
                            'ValR3 = .R
                            'ValG3 = .G
                            'ValB3 = .B
                            'End With


                            SMvalR = ValR1 + SMvalR
                            SMvalG = ValG1 + SMvalG
                            SMvalB = ValB1 + SMvalB



                        Next

                        tempBitmap1.Dispose()
                        tempBitmap1.Dispose()
                        tempBitmap1.Dispose()

                        MDvalR = SMvalR / ((NumericUpDown5.Value))
                        MDvalG = SMvalG / ((NumericUpDown5.Value))
                        MDvalB = SMvalB / ((NumericUpDown5.Value))

                        Dim MDvalTotal As Integer
                        MDvalTotal = MDvalR + MDvalG + MDvalB

                        tx_R_v.Text = MDvalR
                        tx_G_v.Text = MDvalG
                        tx_B_v.Text = MDvalB


                        SinalR(PontoTit) = MDvalR
                        SinalG(PontoTit) = MDvalG
                        SinalB(PontoTit) = MDvalB

                        Chart1.Series(0).Points.AddXY(PontoTit, SinalR(PontoTit))
                        Chart1.Series(1).Points.AddXY(PontoTit, SinalG(PontoTit))
                        Chart1.Series(2).Points.AddXY(PontoTit, SinalB(PontoTit))


                        Chart1.ChartAreas(0).AxisX.Minimum = IIf(Chart1.Series(0).Points.Count > NumPontoTit, Chart1.Series(0).Points.Count - NumPontoTit, 0)
                        Chart1.ChartAreas(0).AxisX.Maximum = IIf(Chart1.Series(0).Points.Count > NumPontoTit, Chart1.Series(0).Points.Count, NumPontoTit)

                        'Verificando limites impostos e soando alarme

                        If cb_R.Checked Then
                            If valR > nup_R.Value Then
                                Timer1.Enabled = False
                                MsgBox("Limite de Vermelho ultrapassado.")
                            End If
                        ElseIf cb_G.Checked Then
                            If valG > nup_G.Value Then
                                Timer1.Enabled = False
                                MsgBox("Limite de Verde ultrapassado.")
                            End If
                        ElseIf cb_B.Checked Then
                            If valB > nup_B.Value Then
                                Timer1.Enabled = False
                                MsgBox("Limite de Azul ultrapassado.")
                            End If
                        End If


                        PontoTit = PontoTit + 1



                        'Salvando os dados:
                        Dim inputString As String = ValoresX.Last & vbTab & MDvalR & vbTab & MDvalG & vbTab & MDvalB & vbNewLine
                        My.Computer.FileSystem.WriteAllText(caminho, inputString, True)

                        TotalissimoB = MDvalB + TotalissimoB

                    End While 'número de pontos da titulação

                    MediaTotalissimoB = TotalissimoB / NumLeituras


                    nova_conc = conc + nup_conc.Value
                    ptsX.Add(nova_conc)
                    ptsY.Add(MediaTotalissimoB)
                    conc = nova_conc
                    counter += 1

                    Chart2.Series(0).Points.AddXY(nova_conc, MediaTotalissimoB)


                   



                Next

                TotalissimoB = 0

                MediaTotalissimoB = 0


            Next

        End While

    End Sub

    Private Sub Button2_Click_1(sender As Object, e As EventArgs) Handles Button2.Click
        flagstop = True
        Application.DoEvents()
        LabelAVISO.Text = "INICIAR"


    End Sub

    Private Sub Timer3_Tick(sender As Object, e As EventArgs) Handles Timer3.Tick
        TimerTick = False
    End Sub

    Private Sub Button3_Click_1(sender As Object, e As EventArgs)
        Dim tempBitmap As Bitmap = New Bitmap(imgVideo.ClientSize.Width, imgVideo.ClientSize.Height)
        imgVideo.DrawToBitmap(tempBitmap, imgVideo.ClientRectangle)


        Dim MeuRetangulo As New RectangleF(PictureBox1.Location, PictureBox1.Size)


        'Copy the selection rectangle from the temporary snapshot to the target bitmap:
        Dim target As New Bitmap(PictureBox1.ClientSize.Width, PictureBox1.ClientSize.Height) 'retangle width and height
        Using g As Graphics = Graphics.FromImage(target)
            g.DrawImage(tempBitmap, target.GetBounds(0), MeuRetangulo, GraphicsUnit.Pixel)
        End Using

        'Clear up: you don't need tempBitmap any more:
        tempBitmap.Dispose()

        'Ready!
        PictureBox1.Image = target



        Dim valR, valG, valB As Integer
        With Helper.GetImageAverageRGB(PictureBox1.Image)
            valR = .R
            valG = .G
            valB = .B
        End With

        tx_R_v.Text = valR
        tx_G_v.Text = valG
        tx_B_v.Text = valB

        Chart1.Series(0).Points.Add(valR)
        Chart1.Series(1).Points.Add(valG)
        Chart1.Series(2).Points.Add(valB)

        Chart1.ChartAreas(0).AxisX.Minimum = IIf(Chart1.Series(0).Points.Count > nPoints_Graph, Chart1.Series(0).Points.Count - nPoints_Graph, 0)
        Chart1.ChartAreas(0).AxisX.Maximum = IIf(Chart1.Series(0).Points.Count > nPoints_Graph, Chart1.Series(0).Points.Count, nPoints_Graph)

        'Verificando limites impostos e soando alarme

        If cb_R.Checked Then
            If valR > nup_R.Value Then
                Timer1.Enabled = False
                MsgBox("Limite de Vermelho ultrapassado.")
            End If
        ElseIf cb_G.Checked Then
            If valG > nup_G.Value Then
                Timer1.Enabled = False
                MsgBox("Limite de Verde ultrapassado.")
            End If
        ElseIf cb_B.Checked Then
            If valB > nup_B.Value Then
                Timer1.Enabled = False
                MsgBox("Limite de Azul ultrapassado.")
            End If
        End If
    End Sub

    Private Sub Button4_Click_1(sender As Object, e As EventArgs) Handles Button4.Click


        'RotinaMulticomut()

        

        Flaginjetar = True


    End Sub

    Private Sub Chart1_Click(sender As Object, e As EventArgs) Handles Chart1.Click

    End Sub

    Private Sub Label19_Click(sender As Object, e As EventArgs)

    End Sub

    Private Sub Button3_Click_2(sender As Object, e As EventArgs) Handles Button3.Click



        PassodeAngulo = NumericUpDown9.Value / 0.1758
        SerialPort1.Write(CStr(PassodeAngulo))
        'EnviaDigitalV(Int)

        ' SerialPort1.Write("1S00" & CStr(NumericUpDown4.Value) & vbCrLf)


    End Sub

    Private Sub NumericUpDown3_ValueChanged(sender As Object, e As EventArgs)

    End Sub

    Private Sub CheckedListBox1_SelectedIndexChanged(sender As Object, e As EventArgs)

    End Sub

    Private Sub CheckBox1_CheckedChanged(sender As Object, e As EventArgs)



        'CheckBox2.Checked = False
        'CheckBox3.Checked = False





    End Sub

    Private Sub CheckBox2_CheckedChanged(sender As Object, e As EventArgs)


        'CheckBox1.Checked = False
        'CheckBox3.Checked = False



    End Sub

    Private Sub CheckBox3_CheckedChanged(sender As Object, e As EventArgs)


        'CheckBox1.Checked = False
        'CheckBox2.Checked = False



    End Sub

    Private Sub Label25_Click(sender As Object, e As EventArgs) Handles Label25.Click

    End Sub

    Private Sub Label27_Click(sender As Object, e As EventArgs) Handles Label27.Click

    End Sub

    Private Sub NumericUpDown6_ValueChanged(sender As Object, e As EventArgs) Handles NumericUpDown6.ValueChanged

        Dim z As Point = New Point(NumericUpDown6.Value, NumericUpDown8.Value)

            PictureBox1.Location = z


    End Sub

    Private Sub NumericUpDown8_ValueChanged(sender As Object, e As EventArgs) Handles NumericUpDown8.ValueChanged

        Dim z As Point = New Point(NumericUpDown6.Value, NumericUpDown8.Value)

            PictureBox1.Location = z


    End Sub

    Private Sub NumericUpDown9_ValueChanged(sender As Object, e As EventArgs) Handles NumericUpDown9.ValueChanged
        PassodeAngulo = NumericUpDown9.Value
    End Sub

    Private Sub PictureBox3_Click(sender As Object, e As EventArgs)

    End Sub

    Private Sub IntervalPR_ValueChanged(sender As Object, e As EventArgs) Handles IntervalPR.ValueChanged
        SerialPort1.PortName = "COM" & CStr(IntervalPR.Value)
    End Sub

    Private Sub Label21_Click(sender As Object, e As EventArgs) Handles Label21.Click

    End Sub

    Private Sub Chart2_Click(sender As Object, e As EventArgs) Handles Chart2.Click

    End Sub

    Private Function FindLinearLeastSquaresFit(ByVal x As List(Of Double), ByVal y As List(Of Double))
        ' Perform the calculation.
        ' Find the values S1, Sx, Sy, Sxx, and Sxy.
        Dim S1 As Integer = x.Count
        Dim Sx As Double = 0
        Dim Sy As Double = 0
        Dim Sxx As Double = 0
        Dim Sxy As Double = 0
        Dim Sqtot As Double = 0
        Dim Sqexp As Double = 0
        Dim ym As Double = y.Sum / S1
        Dim xm As Double = x.Sum / S1
        Dim m As Double = 0
        Dim b As Double = 0
        Dim R2 As Double = 0
        For i = 0 To S1 - 1
            Sx += x(i)
            Sy += y(i)
            Sxx += x(i) * x(i)
            Sxy += x(i) * y(i)
            Sqtot += (y(i) - ym) ^ 2
        Next i
        ' Solve for m and b.
        m = (Sxy * S1 - Sx * Sy) / (Sxx * S1 - Sx * Sx)
        'b = (Sxy * Sx - Sy * Sxx) / (Sx * Sx - S1 * Sxx)
        b = ym - m * xm
        For j = 0 To S1 - 1
            Sqexp += (m * x(j) + b - ym) ^ 2
        Next j
        R2 = Sqexp / Sqtot
        Return {m, b, R2}
    End Function
    Private Sub Button5_Click(sender As Object, e As EventArgs) Handles Button5.Click
        Dim fit As Array = FindLinearLeastSquaresFit(ptsX, ptsY)
        tx_m.Text = fit(0)
        tx_b.Text = fit(1)
        tx_r.Text = fit(2)
    End Sub

    Private Sub LabelAVISO_Click(sender As Object, e As EventArgs) Handles LabelAVISO.Click

    End Sub
End Class
