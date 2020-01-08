#tag Window
Begin Window MainWindow10
   BackColor       =   &cFFFFFF00
   Backdrop        =   0
   CloseButton     =   True
   Compatibility   =   ""
   Composite       =   False
   Frame           =   0
   FullScreen      =   False
   FullScreenButton=   False
   HasBackColor    =   False
   Height          =   521
   ImplicitInstance=   True
   LiveResize      =   True
   MacProcID       =   0
   MaxHeight       =   521
   MaximizeButton  =   True
   MaxWidth        =   421
   MenuBar         =   1632753663
   MenuBarVisible  =   True
   MinHeight       =   521
   MinimizeButton  =   True
   MinWidth        =   421
   Placement       =   1
   Resizeable      =   True
   Title           =   "Word Crush 10x10 - High Score: 0"
   Visible         =   True
   Width           =   421
   Begin PushButton StartButton
      AutoDeactivate  =   True
      Bold            =   False
      ButtonStyle     =   "0"
      Cancel          =   False
      Caption         =   "Start"
      Default         =   True
      Enabled         =   True
      Height          =   20
      HelpTag         =   ""
      Index           =   -2147483648
      InitialParent   =   ""
      Italic          =   False
      Left            =   321
      LockBottom      =   False
      LockedInPosition=   False
      LockLeft        =   True
      LockRight       =   False
      LockTop         =   True
      Scope           =   0
      TabIndex        =   0
      TabPanelIndex   =   0
      TabStop         =   True
      TextFont        =   "System"
      TextSize        =   0.0
      TextUnit        =   0
      Top             =   481
      Transparent     =   False
      Underline       =   False
      Visible         =   True
      Width           =   80
   End
   Begin PushButton ClearButton
      AutoDeactivate  =   True
      Bold            =   False
      ButtonStyle     =   "0"
      Cancel          =   False
      Caption         =   "Clear HS"
      Default         =   False
      Enabled         =   True
      Height          =   20
      HelpTag         =   ""
      Index           =   -2147483648
      InitialParent   =   ""
      Italic          =   False
      Left            =   321
      LockBottom      =   False
      LockedInPosition=   False
      LockLeft        =   True
      LockRight       =   False
      LockTop         =   True
      Scope           =   0
      TabIndex        =   1
      TabPanelIndex   =   0
      TabStop         =   True
      TextFont        =   "System"
      TextSize        =   0.0
      TextUnit        =   0
      Top             =   449
      Transparent     =   False
      Underline       =   False
      Visible         =   True
      Width           =   80
   End
   Begin Label infoLabel
      AutoDeactivate  =   True
      Bold            =   False
      DataField       =   ""
      DataSource      =   ""
      Enabled         =   True
      Height          =   20
      HelpTag         =   ""
      Index           =   -2147483648
      InitialParent   =   ""
      Italic          =   False
      Left            =   20
      LockBottom      =   False
      LockedInPosition=   False
      LockLeft        =   True
      LockRight       =   False
      LockTop         =   True
      Multiline       =   False
      Scope           =   0
      Selectable      =   False
      TabIndex        =   2
      TabPanelIndex   =   0
      TabStop         =   True
      Text            =   "0 words, 0 letters max (0 words)"
      TextAlign       =   1
      TextColor       =   &c00000000
      TextFont        =   "System"
      TextSize        =   0.0
      TextUnit        =   0
      Top             =   481
      Transparent     =   False
      Underline       =   False
      Visible         =   True
      Width           =   289
   End
   Begin Label scoreLabel
      AutoDeactivate  =   True
      Bold            =   False
      DataField       =   ""
      DataSource      =   ""
      Enabled         =   True
      Height          =   20
      HelpTag         =   ""
      Index           =   -2147483648
      InitialParent   =   ""
      Italic          =   False
      Left            =   20
      LockBottom      =   False
      LockedInPosition=   False
      LockLeft        =   True
      LockRight       =   False
      LockTop         =   True
      Multiline       =   False
      Scope           =   0
      Selectable      =   False
      TabIndex        =   3
      TabPanelIndex   =   0
      TabStop         =   True
      Text            =   "196 original remaining, Score: 0"
      TextAlign       =   1
      TextColor       =   &c00000000
      TextFont        =   "System"
      TextSize        =   0.0
      TextUnit        =   0
      Top             =   449
      Transparent     =   False
      Underline       =   False
      Visible         =   True
      Width           =   289
   End
End
#tag EndWindow

#tag WindowCode
	#tag Event
		Sub Close()
		  dim f as FolderItem
		  dim t as TextOutputStream
		  
		  if score > highscore then
		    highscore = score
		  end
		  
		  f = SpecialFolder.Preferences.Child("wc10ud.txt")
		  t = TextOutputStream.Create(f)
		  t.WriteLine str(highscore)
		  t.Close
		  MainWindow14.Show
		  
		End Sub
	#tag EndEvent

	#tag Event
		Sub EnableMenuItems()
		  if undo.Ubound < 0 then
		    EditUndo.Enabled = false
		  else
		    EditUndo.Enabled = true
		  end
		  
		End Sub
	#tag EndEvent

	#tag Event
		Function MouseDown(X As Integer, Y As Integer) As Boolean
		  dim i, j, tempx, tempy as integer
		  
		  if StartButton.Enabled = false then
		    mdx = 0
		    mdy = 0
		    tempx = (x-1) \ 42 + 1
		    tempy = (y-1) \ 42 + 1
		    
		    if (x-1) mod 42 > 0 and (y-1) mod 42 > 0 and tempx > 0 and tempx < 11 and tempy > 0 and tempy < 11 then
		      mdx = tempx
		      mdy = tempy
		      for i = 1 to 10
		        for j = 1 to 10
		          if i=mdx or j=mdy or i-j=mdx-mdy or i+j=mdx+mdy then
		            gridhl(i-1,j-1) = true
		          end
		        next
		      next
		      Refresh
		    end
		    return true
		  else
		    return false
		  end
		  
		End Function
	#tag EndEvent

	#tag Event
		Sub MouseUp(X As Integer, Y As Integer)
		  dim dx,dy,i,j as integer
		  dim s as string
		  
		  if mdx > 0 and mdy > 0 then
		    for i = 1 to 10
		      for j = 1 to 10
		        gridhl(i-1,j-1) = false
		      next
		    next
		    Refresh
		    mux = (x-1) \ 42 + 1
		    muy = (y-1) \ 42 + 1
		    j = max(abs(mdx - mux),abs(mdy - muy))
		    dx = Sign(mux - mdx)
		    dy = sign(muy - mdy)
		    
		    if (x-1) mod 42 > 0 and (y-1) mod 42 > 0 and mux > 0 and mux < 11 and muy > 0 and muy < 11 then
		      if j > 0 and (abs(mdx - mux)=abs(mdy - muy) or abs(mdx - mux)=0 or abs(mdy - muy)=0) then
		        for i = 0 to j
		          s = s + grid(mdx-1+i*dx,mdy-1+i*dy)
		        next
		        if isWord(s) then
		          handleGoodWord(s)
		        else
		          handleBadWord(s)
		        end
		      end
		    end
		  end
		End Sub
	#tag EndEvent

	#tag Event
		Sub Open()
		  dim f as FolderItem
		  dim t as TextInputStream
		  
		  f = SpecialFolder.Preferences.Child("wc10ud.txt")
		  
		  if f.Exists then
		    t = TextInputStream.Open(f)
		    if t <> nil then
		      highscore = val(t.ReadLine)
		      self.Title = "Word Crush 10x10 - High Score: " + str(highscore)
		      t.Close
		    end
		  end
		  
		End Sub
	#tag EndEvent

	#tag Event
		Sub Paint(g As Graphics, areas() As REALbasic.Rect)
		  dim i,j as integer
		  
		  for i=1 to 10
		    for j=1 to 10
		      displayletter g,i,j,grid(i-1,j-1)
		    next
		  next
		  
		End Sub
	#tag EndEvent


	#tag MenuHandler
		Function EditUndo() As Boolean Handles EditUndo.Action
			dim i as integer
			dim item as UndoItem
			
			item = undo.pop
			
			for i = 0 to UBound(item.xyletters)
			if grid(item.xyletters(i).x,item.xyletters(i).y) = "" then
			unplaced = left(unplaced,len(unplaced)-1)
			end
			grid(item.xyletters(i).x,item.xyletters(i).y) = item.xyletters(i).letter
			next
			score = score - item.score
			updateLabels
			Refresh
			
			Return True
			
		End Function
	#tag EndMenuHandler


	#tag Method, Flags = &h0
		Sub clearAction()
		  dim i,j as Integer
		  
		  if score > highscore then
		    highscore = score
		    self.Title = "Word Crush 10x10 - High Score: " + str(highscore)
		  end
		  unplaced = ""
		  for i=0 to 9
		    for j=0 to 9
		      grid(i,j) = ""
		    next
		  next
		  ClearButton.Caption = "Clear HS"
		  StartButton.Enabled = true
		  infoLabel.Text = "0 words, 0 letters max (0 words)"
		  longword = ""
		  Refresh
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub displayletter(g as graphics, x as integer, y as integer, letter as string)
		  if gridhl(x-1,y-1) or (showorig and gridorig(x-1,y-1)) then
		    g.foreColor = rgb(191,191,95)
		  else
		    g.foreColor = rgb(255,255,191)
		  end
		  g.fillrect x*42-40,y*42-40,39,39
		  g.foreColor = rgb(0,0,0)
		  g.TextFont="Courier"
		  g.TextSize=36
		  g.DrawString letter, x*42-32, y*42-9
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub handleBadWord(letters as String)
		  score = score - Pow(2, len(letters)-2)
		  updateLabels
		  refresh
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub handleGoodWord(word as string)
		  dim dx,dy,i,j,rawscore,tempcount as integer
		  dim newundoitem as UndoItem
		  dim newxyletter as XYLetter
		  dim across, down As string
		  dim temp() as string
		  dim aclear(-1) as integer
		  dim dclear(-1) as integer
		  dim scoring as Boolean
		  
		  scoring = false
		  unplaced = unplaced + word
		  j = max(abs(mdx - mux),abs(mdy - muy))
		  dx = Sign(mux - mdx)
		  dy = sign(muy - mdy)
		  newundoitem = new UndoItem
		  for i = 0 to j
		    newxyletter = new XYLetter
		    newxyletter.letter = grid(mdx-1+i*dx,mdy-1+i*dy)
		    newxyletter.x = mdx-1+i*dx
		    newxyletter.y = mdy-1+i*dy
		    newundoitem.xyletters.Append newxyletter
		    if grid(mdx-1+i*dx,mdy-1+i*dy) <> "" then
		      grid(mdx-1+i*dx,mdy-1+i*dy) = ""
		      if gridorig(mdx-1+i*dx,mdy-1+i*dy) then
		        gridorig(mdx-1+i*dx,mdy-1+i*dy) = false
		        origrem = origrem - 1
		        scoring = true
		        if origrem = 0 then
		          gameOver = true
		        end
		      end
		    end
		  next
		  rawscore = len(word) - 2
		  if scoring then
		    if len(word) = letters then
		      newundoitem.score = pow(2,rawscore)
		    else
		      newundoitem.score = rawscore
		    end
		    score = score + newundoitem.score
		  end
		  for i = 1 to 10
		    across = ""
		    down = ""
		    for j = 1 to 10
		      across = across + grid(j-1,i-1)
		      down = down + grid(i-1,j-1)
		    next
		    if across = "" then
		      aclear.Append i
		    end
		    if down = "" then
		      dclear.Append i
		    end
		  next
		  temp = unplaced.Split("")
		  temp.Shuffle
		  tempcount = ubound(temp)+1
		  if (UBound(aclear) < 0 and UBound(dclear) < 0) or (UBound(aclear)+1)*10 + (ubound(dclear)+1) * (9 - UBound(aclear)) > tempcount then
		    undo.Append newundoitem
		  else
		    for i = 0 to UBound(aclear)
		      for j = 1 to 10
		        grid(j-1,aclear(i)-1) = temp.Pop
		      next
		    next
		    for i = 0 to UBound(dclear)
		      for j = 1 to 10
		        if grid(dclear(i)-1,j-1) = "" then
		          grid(dclear(i)-1,j-1) = temp.Pop
		        end
		      next
		    next
		    unplaced = join(temp,"")
		    redim undo(-1)
		  end
		  updateLabels
		  Refresh
		  if gameOver then
		    clearAction
		  end
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Function isWord(word as String) As Boolean
		  dim sql as string
		  sql = "SELECT * from Words WHERE Word='"+word+"'"
		  
		  dim data as RecordSet
		  data = app.wordsDB.SQLSelect(sql)
		  
		  if data.EOF then
		    return false
		  else
		    return true
		  end
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub processWord(word as string, byref words as integer, byref letters as integer)
		  if isWord(word) then
		    words = words + 1
		    if len(word) = letters then
		      longword = longword + ", " + word
		    elseif len(word) > letters then
		      letters = len(word)
		      longword = word
		    end
		  end
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub updateLabels()
		  dim i, x, y, words as integer
		  dim su,sul,sl,sdl,sd,sdr,sr,sur as string
		  
		  letters = 0
		  for x = 0 to 9
		    for y = 0 to 9
		      su = ""
		      sul = ""
		      sl = ""
		      sdl = ""
		      sd = ""
		      sdr = ""
		      sr = ""
		      sur = ""
		      if grid(x,y) <> "" then
		        su = grid(x,y)
		        sul = grid(x,y)
		        sl = grid(x,y)
		        sdl = grid(x,y)
		        sd = grid(x,y)
		        sdr = grid(x,y)
		        sr = grid(x,y)
		        sur = grid(x,y)
		        for i = 1 to 9
		          if y-i > -1 then
		            if grid(x,y-i) <> "" then
		              su = su + grid(x,y-i)
		              processWord(su,words,letters)
		            end
		          end
		          if x-i > -1 and y-i > -1 then
		            if grid(x-i,y-i) <> "" then
		              sul = sul + grid(x-i,y-i)
		              processWord(sul,words,letters)
		            end
		          end
		          if x-i > -1 then
		            if grid(x-i,y) <> "" then
		              sl = sl + grid(x-i,y)
		              processWord(sl,words,letters)
		            end
		          end
		          if x-i > -1 and y+i < 10 then
		            if grid(x-i,y+i) <> "" then
		              sdl = sdl + grid(x-i,y+i)
		              processWord(sdl,words,letters)
		            end
		          end
		          if y+i < 10 then
		            if grid(x,y+i) <> "" then
		              sd = sd + grid(x,y+i)
		              processWord(sd,words,letters)
		            end
		          end
		          if x+i < 10 and y+i < 10 then
		            if grid(x+i,y+i) <> "" then
		              sdr = sdr + grid(x+i,y+i)
		              processWord(sdr,words,letters)
		            end
		          end
		          if x+i < 10 then
		            if grid(x+i,y) <> "" then
		              sr = sr + grid(x+i,y)
		              processWord(sr,words,letters)
		            end
		          end
		          if x+i < 10 and y-i > -1 then
		            if grid(x+i,y-i) <> "" then
		              sur = sur + grid(x+i,y-i)
		              processWord(sur,words,letters)
		            end
		          end
		        next
		      end
		    next
		  next
		  
		  infoLabel.Text = str(words) + if(words=1," word, "," words, ") + str(letters) + " letters max ("+str(longword.CountFields(", ")) + if(longword.CountFields(", ")=1," word"," words")+")"
		  scoreLabel.Text = str(origrem) + " original remaining, Score: " + str(score)
		  if words = 0 then
		    gameOver = true
		  end
		  
		End Sub
	#tag EndMethod


	#tag Property, Flags = &h0
		gameOver As Boolean
	#tag EndProperty

	#tag Property, Flags = &h0
		grid(9,9) As String
	#tag EndProperty

	#tag Property, Flags = &h0
		gridhl(9,9) As Boolean
	#tag EndProperty

	#tag Property, Flags = &h0
		gridorig(9,9) As Boolean
	#tag EndProperty

	#tag Property, Flags = &h0
		highscore As Integer
	#tag EndProperty

	#tag Property, Flags = &h0
		letters As Integer
	#tag EndProperty

	#tag Property, Flags = &h0
		longword As String
	#tag EndProperty

	#tag Property, Flags = &h0
		mdx As Integer
	#tag EndProperty

	#tag Property, Flags = &h0
		mdy As Integer
	#tag EndProperty

	#tag Property, Flags = &h0
		mux As Integer
	#tag EndProperty

	#tag Property, Flags = &h0
		muy As Integer
	#tag EndProperty

	#tag Property, Flags = &h0
		origrem As Integer
	#tag EndProperty

	#tag Property, Flags = &h0
		score As Integer
	#tag EndProperty

	#tag Property, Flags = &h0
		showorig As boolean = false
	#tag EndProperty

	#tag Property, Flags = &h0
		undo() As UndoItem
	#tag EndProperty

	#tag Property, Flags = &h0
		unplaced As String
	#tag EndProperty


	#tag Constant, Name = tiles, Type = String, Dynamic = False, Default = \"AAAAAAAAABBCCDDDDEEEEEEEEEEEEFFGGGHHIIIIIIIIIJKLLLLMMNNNNNNOOOOOOOOPPQRRRRRRSSSSTTTTTTUUUUVVWWXYYZ", Scope = Public
	#tag EndConstant


#tag EndWindowCode

#tag Events StartButton
	#tag Event
		Sub Action()
		  dim temp(-1) As string
		  dim i,j as integer
		  
		  for i = 1 to 100
		    temp.Append mid(tiles,i,1)
		  next
		  
		  temp.Shuffle
		  for i=0 to 9
		    for j=0 to 9
		      grid(i,j) = temp.Pop
		      gridorig(i,j) = true
		    next
		  next
		  origrem = 100
		  score = 0
		  gameOver = false
		  updateLabels
		  Refresh
		  ClearButton.Caption = "Clear"
		  me.Enabled = false
		  
		End Sub
	#tag EndEvent
#tag EndEvents
#tag Events ClearButton
	#tag Event
		Sub Action()
		  dim f as FolderItem
		  dim t as TextOutputStream
		  
		  if me.Caption = "Clear" then
		    clearAction
		  else
		    score = 0
		    highscore = 0
		    f = SpecialFolder.Preferences.Child("wc10ud.txt")
		    t = TextOutputStream.Create(f)
		    t.WriteLine str(0)
		    t.Close
		    self.Title = "Word Crush 10x10 - High Score: " + str(highscore)
		  end
		  
		End Sub
	#tag EndEvent
#tag EndEvents
#tag Events infoLabel
	#tag Event
		Function MouseDown(X As Integer, Y As Integer) As Boolean
		  if longword <> "" then
		    
		    dim longpop as new MenuItem
		    dim temp() as string
		    dim i as integer
		    
		    temp = longword.split(", ")
		    temp.Sort
		    for i = 0 to UBound(temp)
		      longpop.Append (new MenuItem(temp(i)))
		    next
		    
		    longpop = longpop.PopUp
		    
		  end
		  
		End Function
	#tag EndEvent
#tag EndEvents
#tag Events scoreLabel
	#tag Event
		Function MouseDown(X As Integer, Y As Integer) As Boolean
		  showorig = true
		  Refresh
		  return true
		  
		End Function
	#tag EndEvent
	#tag Event
		Sub MouseUp(X As Integer, Y As Integer)
		  showorig = false
		  Refresh
		  
		End Sub
	#tag EndEvent
#tag EndEvents
#tag ViewBehavior
	#tag ViewProperty
		Name="Name"
		Visible=true
		Group="ID"
		Type="String"
		EditorType="String"
	#tag EndViewProperty
	#tag ViewProperty
		Name="Interfaces"
		Visible=true
		Group="ID"
		Type="String"
		EditorType="String"
	#tag EndViewProperty
	#tag ViewProperty
		Name="Super"
		Visible=true
		Group="ID"
		Type="String"
		EditorType="String"
	#tag EndViewProperty
	#tag ViewProperty
		Name="Width"
		Visible=true
		Group="Size"
		InitialValue="600"
		Type="Integer"
	#tag EndViewProperty
	#tag ViewProperty
		Name="Height"
		Visible=true
		Group="Size"
		InitialValue="400"
		Type="Integer"
	#tag EndViewProperty
	#tag ViewProperty
		Name="MinWidth"
		Visible=true
		Group="Size"
		InitialValue="64"
		Type="Integer"
	#tag EndViewProperty
	#tag ViewProperty
		Name="MinHeight"
		Visible=true
		Group="Size"
		InitialValue="64"
		Type="Integer"
	#tag EndViewProperty
	#tag ViewProperty
		Name="MaxWidth"
		Visible=true
		Group="Size"
		InitialValue="32000"
		Type="Integer"
	#tag EndViewProperty
	#tag ViewProperty
		Name="MaxHeight"
		Visible=true
		Group="Size"
		InitialValue="32000"
		Type="Integer"
	#tag EndViewProperty
	#tag ViewProperty
		Name="Frame"
		Visible=true
		Group="Frame"
		InitialValue="0"
		Type="Integer"
		EditorType="Enum"
		#tag EnumValues
			"0 - Document"
			"1 - Movable Modal"
			"2 - Modal Dialog"
			"3 - Floating Window"
			"4 - Plain Box"
			"5 - Shadowed Box"
			"6 - Rounded Window"
			"7 - Global Floating Window"
			"8 - Sheet Window"
			"9 - Metal Window"
			"11 - Modeless Dialog"
		#tag EndEnumValues
	#tag EndViewProperty
	#tag ViewProperty
		Name="Title"
		Visible=true
		Group="Frame"
		InitialValue="Untitled"
		Type="String"
	#tag EndViewProperty
	#tag ViewProperty
		Name="CloseButton"
		Visible=true
		Group="Frame"
		InitialValue="True"
		Type="Boolean"
		EditorType="Boolean"
	#tag EndViewProperty
	#tag ViewProperty
		Name="Resizeable"
		Visible=true
		Group="Frame"
		InitialValue="True"
		Type="Boolean"
		EditorType="Boolean"
	#tag EndViewProperty
	#tag ViewProperty
		Name="MaximizeButton"
		Visible=true
		Group="Frame"
		InitialValue="True"
		Type="Boolean"
		EditorType="Boolean"
	#tag EndViewProperty
	#tag ViewProperty
		Name="MinimizeButton"
		Visible=true
		Group="Frame"
		InitialValue="True"
		Type="Boolean"
		EditorType="Boolean"
	#tag EndViewProperty
	#tag ViewProperty
		Name="FullScreenButton"
		Visible=true
		Group="Frame"
		InitialValue="False"
		Type="Boolean"
		EditorType="Boolean"
	#tag EndViewProperty
	#tag ViewProperty
		Name="Composite"
		Group="OS X (Carbon)"
		InitialValue="False"
		Type="Boolean"
	#tag EndViewProperty
	#tag ViewProperty
		Name="MacProcID"
		Group="OS X (Carbon)"
		InitialValue="0"
		Type="Integer"
	#tag EndViewProperty
	#tag ViewProperty
		Name="FullScreen"
		Group="Behavior"
		InitialValue="False"
		Type="Boolean"
		EditorType="Boolean"
	#tag EndViewProperty
	#tag ViewProperty
		Name="ImplicitInstance"
		Visible=true
		Group="Behavior"
		InitialValue="True"
		Type="Boolean"
		EditorType="Boolean"
	#tag EndViewProperty
	#tag ViewProperty
		Name="LiveResize"
		Group="Behavior"
		InitialValue="True"
		Type="Boolean"
		EditorType="Boolean"
	#tag EndViewProperty
	#tag ViewProperty
		Name="Placement"
		Visible=true
		Group="Behavior"
		InitialValue="0"
		Type="Integer"
		EditorType="Enum"
		#tag EnumValues
			"0 - Default"
			"1 - Parent Window"
			"2 - Main Screen"
			"3 - Parent Window Screen"
			"4 - Stagger"
		#tag EndEnumValues
	#tag EndViewProperty
	#tag ViewProperty
		Name="Visible"
		Visible=true
		Group="Behavior"
		InitialValue="True"
		Type="Boolean"
		EditorType="Boolean"
	#tag EndViewProperty
	#tag ViewProperty
		Name="HasBackColor"
		Visible=true
		Group="Background"
		InitialValue="False"
		Type="Boolean"
	#tag EndViewProperty
	#tag ViewProperty
		Name="BackColor"
		Visible=true
		Group="Background"
		InitialValue="&hFFFFFF"
		Type="Color"
	#tag EndViewProperty
	#tag ViewProperty
		Name="Backdrop"
		Visible=true
		Group="Background"
		Type="Picture"
		EditorType="Picture"
	#tag EndViewProperty
	#tag ViewProperty
		Name="MenuBar"
		Visible=true
		Group="Menus"
		Type="MenuBar"
		EditorType="MenuBar"
	#tag EndViewProperty
	#tag ViewProperty
		Name="MenuBarVisible"
		Visible=true
		Group="Deprecated"
		InitialValue="True"
		Type="Boolean"
		EditorType="Boolean"
	#tag EndViewProperty
	#tag ViewProperty
		Name="unplaced"
		Group="Behavior"
		Type="String"
		EditorType="MultiLineEditor"
	#tag EndViewProperty
	#tag ViewProperty
		Name="mdx"
		Group="Behavior"
		Type="Integer"
	#tag EndViewProperty
	#tag ViewProperty
		Name="mdy"
		Group="Behavior"
		Type="Integer"
	#tag EndViewProperty
	#tag ViewProperty
		Name="mux"
		Group="Behavior"
		Type="Integer"
	#tag EndViewProperty
	#tag ViewProperty
		Name="muy"
		Group="Behavior"
		Type="Integer"
	#tag EndViewProperty
	#tag ViewProperty
		Name="longword"
		Group="Behavior"
		Type="String"
		EditorType="MultiLineEditor"
	#tag EndViewProperty
	#tag ViewProperty
		Name="origrem"
		Group="Behavior"
		Type="Integer"
	#tag EndViewProperty
	#tag ViewProperty
		Name="score"
		Group="Behavior"
		Type="Integer"
	#tag EndViewProperty
	#tag ViewProperty
		Name="gameOver"
		Group="Behavior"
		Type="Boolean"
	#tag EndViewProperty
	#tag ViewProperty
		Name="showorig"
		Group="Behavior"
		InitialValue="false"
		Type="boolean"
	#tag EndViewProperty
	#tag ViewProperty
		Name="letters"
		Group="Behavior"
		Type="Integer"
	#tag EndViewProperty
	#tag ViewProperty
		Name="highscore"
		Group="Behavior"
		Type="Integer"
	#tag EndViewProperty
#tag EndViewBehavior
