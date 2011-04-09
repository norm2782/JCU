class RulesTreeView extends Backbone.View

  id: 'rules-tree-view'

  initialize: ->
    app.collections.rules-tree.bind 'add', @addOne
    app.collections.rules-tree.bind 'refresh', @addAll
    app.collections.rules-tree.bind 'all', @renderStats

  render: ->
    $(@el).html app.templates.rules-tree()
    @

  addOne: (rule) =>
    view = new RuleTreeView model: rule
    $(@el).find("#rules-tree").append view.render().el

  addAll: =>
    app.collections.rules-tree.each @addOne

  renderStats: =>
    app.views.stats.render()

