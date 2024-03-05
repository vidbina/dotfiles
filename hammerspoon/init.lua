cheatsheet = ""
-- A dummy binding for Hammerspoon
hs.hotkey.bind({"cmd", "alt", "ctrl"}, "H", function()
    hs.alert.show(cheatsheet)
    hs.notify.new({title="Hammerspoon", informativeText=cheatsheet}):send()
end)
cheatsheet += "\n⌃⌥⌘ H = help"

-- https://www.hammerspoon.org/go/#simple-configuration-reloading
hs.hotkey.bind({"cmd", "alt", "ctrl"}, "R", function()
    hs.alert.show("Reloading Hammerspoon config")
    hs.reload()
end)
cheatsheet += "\n⌃⌥⌘ R = reload"

-- https://www.hammerspoon.org/go/#drawing-on-the-screen
mouseCircle = nil
mouseCircleTimer = nil

function mouseHighlight()
    -- Delete an existing highlight if it exists
    if mouseCircle then
        mouseCircle:delete()
        if mouseCircleTimer then
            mouseCircleTimer:stop()
        end
    end
    -- Get the current co-ordinates of the mouse pointer
    mousepoint = hs.mouse.absolutePosition()
    -- Prepare a big red circle around the mouse pointer
    mouseCircle = hs.drawing.circle(hs.geometry.rect(mousepoint.x-40, mousepoint.y-40, 80, 80))
    mouseCircle:setStrokeColor({["red"]=1,["blue"]=0,["green"]=0,["alpha"]=1})
    mouseCircle:setFill(false)
    mouseCircle:setStrokeWidth(5)
    mouseCircle:show()

    -- Set a timer to delete the circle after 3 seconds
    mouseCircleTimer = hs.timer.doAfter(3, function()
      mouseCircle:delete()
      mouseCircle = nil
    end)
end
hs.hotkey.bind({"cmd","alt","shift"}, "D", mouseHighlight)
cheatsheet += "\n⌃⌥⌘ R = reload"