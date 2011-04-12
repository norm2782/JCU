class Rule extends Backbone.Model

  defaults:
    term: 'empty rule...'

  clear: ->
    @destroy()
    @view.remove()
