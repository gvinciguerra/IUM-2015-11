open System
open System.Windows.Forms
open System.Drawing
open System.Drawing.Drawing2D
open Microsoft.Win32

/// Implementa un oggetto che permette animare elementi grafici attraverso l'interpolazione lineare di due matrici di trasformazione.
type MatrixAnimator() =
    let mutable currentMatrix = new Matrix()
    let mutable startingMatrix = new Matrix()
    let mutable endingMatrix = new Matrix()
    let mutable startTime = System.DateTime.Now
    let mutable lastTime = System.DateTime.Now
    let mutable duration = 0.f
    let mutable progress = 1.f
    let timerInterval = 30.f
    let timer = new Timer(Interval=int timerInterval)
    let didChangeEvent = new Event<System.EventArgs>()

    let animate() =
        let now = System.DateTime.Now
        let dt = single (now - startTime).TotalMilliseconds
        progress <- 1.f - (duration - dt) / duration

        if progress >= 1.f then
            currentMatrix <- endingMatrix
            timer.Stop()
        else
            let m1 = endingMatrix.Elements |> Array.map (fun t -> progress * t)
            let m2 = startingMatrix.Elements |> Array.map (fun t -> (1.f - progress) * t)
            let m3 = Array.map2 (fun a b -> a + b) m1 m2
            currentMatrix <- new Matrix(m3.[0], m3.[1], m3.[2], m3.[3], m3.[4], m3.[5])
            lastTime <- now
    do timer.Tick.Add(fun e  -> animate() ; didChangeEvent.Trigger(e))

    /// Si verifica quando la matrice corrente viene aggiornata.
    member this.MatrixDidChange = didChangeEvent.Publish

    /// Restituisce la matrice corrente.
    member this.Matrix = currentMatrix

    /// Un valore compreso tra 0 e 1 che indica lo stato dell'animazione.
    member this.Progress with get() = progress

    /// Inizia l'animazione. La durata dell'animazione è in millisecondi.
    member this.StartAnimation(ms : single, m1 : Matrix, m2 : Matrix) =
        timer.Stop()
        assert (ms > timerInterval)
        duration <- ms
        startingMatrix <- m1
        endingMatrix <- m2
        lastTime <- System.DateTime.Now
        startTime <- System.DateTime.Now
        currentMatrix <- m1
        startingMatrix <- m1
        endingMatrix <- m2
        timer.Start()

type DesktopWallpaperBox() as this =
    inherit UserControl()
    
    let mutable image = null
    do
        let key = Registry.CurrentUser.OpenSubKey("Control Panel\\Desktop")
        if key <> null then
            let path = key.GetValue("WallPaper").ToString()
            image <- new Bitmap(path)

    do this.SetStyle(ControlStyles.OptimizedDoubleBuffer ||| ControlStyles.AllPaintingInWmPaint, true)

    let mutable transform = new Matrix()
    member this.Transform with get() = transform and set(v) = transform <- v ; this.Invalidate()

    override this.OnPaint e =
        let g = e.Graphics
        g.Transform <- transform
        let x, y, width, height = 5.f, 5.f, 150.f, 150.f
        if image <> null then
            g.DrawImage(image, x, y, width, height)
        else
            use b = new SolidBrush(Color.DarkBlue)
            g.FillEllipse(b, x, y, width, height)

let f = new Form(Text="Matrix Animator", TopMost=true)
let image = new DesktopWallpaperBox(Dock=DockStyle.Fill, Location=new Point(5, 5), Size=new Size(150, 150))

let anim = new MatrixAnimator()
let animateWith i =
    let m1 = new Matrix()
    let m2 = new Matrix()
    let rnd = new Random()
    match i with 
    | 0 -> m2.RotateAt(single(rnd.Next(-360, 360)), new PointF(single image.Width / 2.f, single image.Height / 2.f))
    | 1 -> m2.Scale(single(rnd.Next(1, 300)) / 100.f, single(rnd.Next(1, 300)) / 100.f)
    | _ -> m2.Translate(single(rnd.Next(200)), single(rnd.Next(200)))
    anim.MatrixDidChange.Add(fun _ -> image.Transform <- anim.Matrix)
    anim.StartAnimation(3000.f, m1, m2)

let btnRotate = new Button(Text="Random Rotate", Dock=DockStyle.Bottom)
let btnScale = new Button(Text="Random Scale", Dock=DockStyle.Bottom)
let btnTranslate = new Button(Text="Random Translate", Dock=DockStyle.Bottom)
btnRotate.Click.Add(fun _ -> animateWith 0)
btnScale.Click.Add(fun _ -> animateWith 1)
btnTranslate.Click.Add(fun _ -> animateWith 2)
f.Controls.Add(btnRotate)
f.Controls.Add(btnScale)
f.Controls.Add(btnTranslate)
f.Controls.Add(image)
f.Show()
Application.Run(f)