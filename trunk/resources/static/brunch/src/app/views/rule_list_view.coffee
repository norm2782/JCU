class RulesListView extends Backbone.View

  id: 'rules-list-view'

  initialize: ->
    app.collections.rules-list.bind 'add', @addOne
    app.collections.rules-list.bind 'refresh', @addAll
    app.collections.rules-list.bind 'all', @renderStats

  render: ->
    $(@el).html app.templates.rules-list()
    @

  addOne: (rule) =>
    view = new RuleListView model: rule
    $(@el).find("#rules-list").append view.render().el

  addAll: =>
    app.collections.rules-list.each @addOne

  renderStats: =>
    app.views.stats.render()
