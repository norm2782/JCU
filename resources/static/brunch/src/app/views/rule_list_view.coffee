class RulesListView extends Backbone.View

  id: 'rules-list-view'

  initialize: ->
    app.collections.rulesList.bind 'add', @addOne
    app.collections.rulesList.bind 'refresh', @addAll
    app.collections.rulesList.bind 'all', @renderStats

  render: ->
    $(@el).html app.templates.rulesList()
    @

  addOne: (rule) =>
    view = new RulesListView model: rule
    $(@el).find("#rules-list").append view.render().el

  addAll: =>
    app.collections.rulesList.each @addOne

  renderStats: =>
    app.views.stats.render()
