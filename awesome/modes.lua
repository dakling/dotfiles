-- implement modes like i3
modeFunction = function (modekeys)
  root.keys(modekeys)
end
backToNormalMode = function ()
  root.keys(globalkeys)
end
