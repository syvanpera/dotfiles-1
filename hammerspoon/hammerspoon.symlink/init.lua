hs.notify.new({title="Hammerspoon", informativeText="Config loaded"}):send()

hs.loadSpoon("ReloadConfiguration")
spoon.ReloadConfiguration:start()

local hotkeyModifiers = {"cmd", "alt", "ctrl"}

hs.window.animationDuration = 0

hs.grid.GRIDWIDTH = 6
hs.grid.GRIDHEIGHT = 8
hs.grid.MARGINX = 0
hs.grid.MARGINY = 0

local gw = hs.grid.GRIDWIDTH
local gh = hs.grid.GRIDHEIGHT

local goLeft = {x = 0, y = 0, w = gw/2, h = gh}
local goLeftWide = {x = 0, y = 0, w = gw/2 + 1, h = gh}
local goRight = {x = gw/2, y = 0, w = gw/2, h = gh}
local goRightWide = {x = gw/2 - 1, y = 0, w = gw/2 + 1, h = gh}
local goMaximized = {x = 0, y = 0, w = gw, h = gh}

windowStates = {}

hs.hotkey.bind(hotkeyModifiers, "F", function()
  local win = hs.window.focusedWindow()
  win:toggleFullscreen()
end)

hs.hotkey.bind(hotkeyModifiers, "O", function()
  local win = hs.window.focusedWindow()
  local currentScreen = win:screen()
  local otherScreen = currentScreen:toEast() or currentScreen:toWest()
  win:moveToScreen(otherScreen)
end)

hs.hotkey.bind(hotkeyModifiers, "H", function()
  local win = hs.window.focusedWindow()
  if win then
    local winId = win:id()
    local nextState = goLeft
    if windowStates[winId] == "left" then
      nextState = goLeftWide
      windowStates[winId] = "leftWide"
    else
      nextState = goLeft
      windowStates[winId] = "left"
    end
    hs.grid.set(win, nextState, win:screen())
  end
end)

hs.hotkey.bind(hotkeyModifiers, "L", function()
  local win = hs.window.focusedWindow()
  if win then
    local winId = win:id()
    local nextState = goRight
    if windowStates[winId] == "right" then
      nextState = goRightWide
      windowStates[winId] = "rightWide"
    else
      nextState = goRight
      windowStates[winId] = "right"
    end
    hs.grid.set(win, nextState, win:screen())
  end
end)

hs.hotkey.bind(hotkeyModifiers, "X", function()
  local win = hs.window.focusedWindow()
  if win then
    hs.grid.set(win, goMaximized, win:screen())
  end
end)

hs.hotkey.bind(hotkeyModifiers, "D", function()
  local win = hs.window.focusedWindow()
  hs.alert.show(win:id())
end)
