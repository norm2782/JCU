class RulesTreeView extends Backbone.View

  id: 'rules-tree-view'
  tagName: 'ul'
  isTerm: true

  initialize: ->
    _.bindAll(@, 'addOne', 'addAll', 'render')
    app.collections.rulesTree.bind 'add', @addOne
    app.collections.rulesTree.bind 'refresh', @addAll
    app.collections.rulesTree.bind 'all', @renderList
    app.collections.rulesTree.fetch()

  addOne: (rule) =>
    view = new RulesTreeItemView model: rule, isTerm: @isTerm
    this.isTerm = !this.isTerm
    $(@el).append view.render().el

  addAll: =>
    app.collections.rulesTree.each @addOne

  renderList: =>
    app.views.rulesTree.render()

