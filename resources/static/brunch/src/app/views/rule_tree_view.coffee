class RulesTreeView extends Backbone.View

  id: 'rules-tree-view'

  initialize: ->
    app.collections.rulesTree.bind 'add', @addOne
    app.collections.rulesTree.bind 'refresh', @addAll
    app.collections.rulesTree.bind 'all', @renderStats

  render: ->
    $(@el).html app.templates.rulesTree()
    @

  addOne: (rule) =>
    view = new RulesTreeView model: rule
    $(@el).find("#rules-tree").append view.render().el

  addAll: =>
    app.collections.rulesTree.each @addOne

  renderStats: =>
    app.views.stats.render()
