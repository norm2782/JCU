class RulesTree extends Backbone.Collection

  model: Rule

  done: ->
    return @filter( (rule) ->
      rule.get 'done'
    )

  remaining: ->
    @without.apply @, @done()

  nextOrder: ->
    return 1 unless @length
    @last().get('order') + 1

  comparator: (rule) ->
    rule.get 'order'

  clearCompleted: ->
    _.each(@done(), (rule) ->
      rule.clear()
    )
