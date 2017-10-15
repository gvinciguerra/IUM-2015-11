module VectorEditorApp.Widgets
open VectorEditorApp.LWCs
open System.Windows.Forms
open System.Drawing
open System.Drawing.Drawing2D
open System.Drawing.Text

type CuteButtonType = PushOnPushOff | Push
and CuteButton() =
    inherit LWC()

    let clickevt = new Event<System.EventArgs>()
    let downevt = new Event<MouseEventArgs>()
    let upevt = new Event<MouseEventArgs>()
    let moveevt = new Event<MouseEventArgs>()
    let mutable text = ""
    let mutable pressed = false
    let mutable stringSize = new SizeF()
    let mutable buttonType = Push
    let mutable font = new Font(FontFamily.GenericSansSerif, 10.f)

    member this.Size
        with get() = base.Size
        and set(v) =
            base.Size <- v 
            let fontSize = v.Height - v.Height / 5.f
            font <- new Font(FontFamily.GenericSansSerif,  fontSize, GraphicsUnit.Pixel)

    member this.Pressed
        with get() = pressed
        and set(v) = pressed <- v; this.Invalidate()

    member this.ButtonType
        with get() = buttonType
        and set(v) = pressed <- false; buttonType <- v; this.Invalidate()

    member this.Text
        with get() = text
        and set(v) = text <- v; this.Invalidate()

    member this.Rectangle : Rectangle = Rectangle.Round(new RectangleF(this.Location, this.Size))

    member val Tag = -1 with get, set

    // BUTTON STYLE
    member val Brush = new SolidBrush(Color.FromArgb(83, 83, 83)) :> Brush with get, set
    member val HoverBrush = new SolidBrush(Color.FromArgb(100, 100, 100)) :> Brush with get, set
    member val ActiveBrush = new SolidBrush(Color.FromArgb(40, 40, 40)) :> Brush with get, set

    // MOUSE ACTIONS
    member this.Click = clickevt.Publish
    member this.MouseDown = downevt.Publish
    member this.MouseUp = upevt.Publish
    member this.MouseMove = moveevt.Publish

    override this.OnMouseUp e =
        match buttonType with 
        | Push -> pressed <- false; this.Invalidate(); upevt.Trigger(e);
        | _ -> ()
        clickevt.Trigger(new System.EventArgs())

    override this.OnMouseMove e =
        this.Invalidate()
        moveevt.Trigger(e)

    override this.OnMouseDown e =
        match buttonType with 
        | PushOnPushOff  -> pressed <- not pressed; this.Invalidate(); downevt.Trigger(e)
        | Push -> pressed <- true; this.Invalidate(); downevt.Trigger(e);
    
    // PAINT METHOD
    override this.OnPaint e =
        let g = e.Graphics
        let strSize = g.MeasureString(text, font)
        let strPos  = PointF((this.Size.Width - strSize.Width) / 2.f, (this.Size.Height - strSize.Height) / 2.f)

        let isMouseInside = this.Rectangle.Contains(this.Parent.PointToClient(Cursor.Position))
        let color = if this.Pressed then this.ActiveBrush else if isMouseInside then this.HoverBrush else this.Brush
        g.TextRenderingHint <- TextRenderingHint.AntiAliasGridFit
        g.FillRectangle(color, 0.f, 0.f, this.Size.Width, this.Size.Height)
        g.DrawString(text, font, Brushes.White, strPos)


type BouncingBall() =
    let mutable location = PointF()
    let mutable speed = SizeF(10.f,10.f) // px/s
    let mutable size = SizeF(25.f,25.f)
    let mutable lastT = System.DateTime.Now
    let path = new GraphicsPath()
    let mutable previousLocation = PointF()

    let updatePath() =
        path.Reset()
        path.AddEllipse(new RectangleF(location, size))

    member this.Location with get() = location and set(v) = previousLocation <- location ; location <- v ; updatePath()
    member this.Speed with get() = speed and set(v) = speed <- v
    member this.Size with get() = size and set(v) = size <- v ; updatePath()
    member this.Bounds = new RectangleF(location, size)
    member this.Center with get() = PointF(location.X + size.Width / 2.f, location.Y + size.Height / 2.f)
    member this.Path with get() = path
    member this.ResetPreviousLocation() = location <- previousLocation

    /// Individua le collisioni della pallina con una collezione di palline. Aggiorna la velocità di conseguenza.
    member this.UpdateSpeed(balls:BouncingBall seq) =
        let vb (b:BouncingBall) (vx : single, vy : single) =
            let ct, cb = this.Center, b.Center
            let cx, cy = cb.X - ct.X, cb.Y - ct.Y
            let sp = cx*vx + cy*vy
            let cr, vr = sqrt(cx*cx + cy*cy), sqrt(vx*vx+vy*vy)
            let cosa = sp / (cr*vr)
            let sina = sqrt(1.f - cosa*cosa)
            let vtb, votb = - vr * cosa, vr * sina
            let beta = atan2 cy  cx
            let pi = single(System.Math.PI)
            let vtbx, vtby = vtb * cos(beta), vtb * sin(beta)
            let votbx, votby = votb * cos(pi / 2.f - beta), votb * sin(pi / 2.f - beta)
            (votbx + vtbx, votby + vtby)
    
        for b in balls do
            if b <> this && this.CollideWith(b) then
                let vx, vy = vb b (speed.Width, speed.Height)
                let swap = SizeF(this.Speed.Width, this.Speed.Height)
                this.Speed <- SizeF(vx, vy)
                b.Speed <- swap
                // Evita che le palline si compenetrino
                this.ResetPreviousLocation()
                b.ResetPreviousLocation()

    /// Aggiorna la posizione della pallina in base al tempo corrente e al vettore velocità.
    member this.UpdatePosition() =
        let t = System.DateTime.Now
        let dt = t - lastT
        let vx = speed.Width / 1000.f
        let vy = speed.Height / 1000.f
        let dx = vx * single dt.TotalMilliseconds
        let dy = vy * single dt.TotalMilliseconds
        this.Location <- PointF(location.X + dx, location.Y + dy)
        lastT <- t
        updatePath()

    /// Ritorna true se la pallina e b si sovrappongono.
    member this.CollideWith (b : BouncingBall) =
        let sqdist (p1 : PointF) (p2 : PointF) =
            let dx, dy = p1.X - p2.X, p1.Y - p2.Y
            dx ** 2.f + dy ** 2.f
        let c1, c2 = this.Center, b.Center
        let d1, d2 = this.Size.Width / 2.f, b.Size.Width / 2.f
        sqdist c1 c2 <= (d1 + d2) * (d1 + d2)

    /// Aggiorna la velocità della pallina calcolando la distanza tra Location e il punto fornito.
    member this.ThrowBall ((p : Point), (multiplier : single)) =
        let c, x, y = this.Center, single p.X, single p.Y
        this.Speed <- SizeF((x - c.X) * multiplier, (y - c.Y) * multiplier)
        updatePath()

    member this.Paint (g : Graphics) =
        g.FillPath(Brushes.Red, path)
        g.DrawPath(Pens.DarkRed, path)