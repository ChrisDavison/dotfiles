Gui, Add, Text,, Choose a bookmark...
Gui, Add, ListBox, vMyListBox gMyListBox w640 r10

bookmarks := []
bookmarks.Push("Hacker News;https://news.ycombinator.com")
bookmarks.Push("Lobsters;https://lobste.rs")
bookmarks.Push("BBC News;https://bbc.co.uk/news")
bookmarks.Push("Youtube;https://www.youtube.com")
bookmarks.Push("strath email;https://outlook.office365.com/mail/inbox")
bookmarks.Push("slack;https://cidcom.slack.com")
bookmarks.Push("weather - darksky;https://darksky.net/forecast/55.8611,-4.2502/uk212/en")
; bookmarks.Push("Wikipedia;https://www.wikipedia.com")
bookmarks.Push("YT Subscriptions;https://www.youtube.com/feed/subscriptions")
bookmarks.Push("YT Asmr;https://www.youtube.com/playlist?list=PLHCsA_6Hf0ebZEZrdTkL5IwizYOYmLnNc")
bookmarks.Push("YT Watch Later;https://www.youtube.com/playlist?list=WL")
; bookmarks.Push("SSSS Comic;http://sssscomic.com/")

for index, element in bookmarks {
    el:=element
    name:=RegexReplace(el, ";.*")
    GuiControl,, MyListBox, %name%
}

Gui, Show
return

MyListBox:
GuiControlGet, MyListBox  ; Retrieve the ListBox's current selection.
choice:=MyListBox
line_for_choice:=""
for index, element in bookmarks {
    el:=element
    name:=RegexReplace(el, ";.*")
    ;MsgBox,,, %name% . " " . %choice%
    If(name = choice) {
        line_for_choice := element
        break
    }
}
choice:=line_for_choice
url := RegExReplace(choice, ".*;")
run %url%
ExitApp
return

GuiClose:
GuiEscape:
ExitApp
