class Rule extends Backbone.Model

  defaults:
    content: 'empty rule...'
    isTerm: true

  clear: ->
    @destroy()
    @view.remove()
