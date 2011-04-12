class RulesListView extends Backbone.View

  id: 'rules-list-view'

  initialize: ->
    _.bindAll(@, 'addOne', 'addAll', 'render')

    app.collections.rulesList.bind 'add', @addOne
    app.collections.rulesList.bind 'refresh', @addAll
    app.collections.rulesList.bind 'all', @renderList
    app.collections.rulesList.fetch()

  render: ->
    console.log "RulesListView.render"
    console.log app.collections.rulesList
    $(@el).html app.templates.rulesList()
    @

  addOne: (rule) =>
    console.log "RulesListView.addOne"
    view = new RulesListItemView model: rule
    $(@el).find("#rules-list").append view.render().el

  addAll: =>
    console.log "RulesListView.addAll"
    app.collections.rulesList.each @addOne

  renderList: =>
    console.log "RulesListView.renderList"
    app.views.rulesList.render()
