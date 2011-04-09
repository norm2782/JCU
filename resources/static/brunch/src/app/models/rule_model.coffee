class Rule extends Backbone.Model

  defaults:
    content: 'empty rule...'

  clear: ->
    @destroy()
    @view.remove()
