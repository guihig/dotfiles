Variable.g({
    coq_settings = {
        auto_start = "shut-up",
        clients = {['tmux.enabled'] = false, ['snippets.warn'] = {}},
        display = {
            icons = {mode = "short"},
            pum = {kind_context = {" [ ", " ] "}, source_context = {"", ""}}
        }
    }
})

require('coq_3p') {{src = 'nvimlua', short_name = 'nLUA'}}
