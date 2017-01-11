# Your init script
#
# Atom will evaluate this file each time a new window is opened. It is run
# after packages are loaded/activated and after the previous editor state
# has been restored.
#
# An example hack to log to the console when each text editor is saved.
#
# atom.workspace.observeTextEditors (editor) ->
#   editor.onDidSave ->
#     console.log "Saved! #{editor.getPath()}"
process.env.PATH = ["/usr/bin", "/usr/local/bin"].join(":")

atom.commands.add 'body', 'cd:swap-theme', ->
    tm = atom.themes
    currentMode = atom.config.get('core.themeMode')
    if currentMode != 'dark'
        atom.config.set('core.themes', ["nord-atom-syntax", "nord-atom-ui"])
        atom.config.set('core.themeMode', 'dark')
    else
        atom.config.set('core.themes', ["katana-syntax", "atom-material-ui"])
        atom.config.set('core.themeMode', 'light')

RunGoMD = () ->
    childProcess = require 'child_process'
    editor = atom.workspace.getActiveTextEditor()
    from_path = editor.getPath()
    cwd = from_path.substr(0, from_path.lastIndexOf('\\') + 1)
    gomd = childProcess.spawn 'gomd',[from_path], {cwd}
    gomd.stdout.on 'data', (d) -> console.log('stdout: ' + d)
    gomd.stderr.on 'data', (d) -> console.log('stderr: ' + d)
    gomd.on 'close', (c) ->
        if (c == 0)
            atom.notifications.addSuccess('GoMD succeeded.')
        else
            atom.notifications.addError('GoMD failed.')

atom.commands.add 'atom-text-editor', 'cd:gomd': ->
    RunGoMD()
