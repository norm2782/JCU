RulesTreeNodeView = require('views/rules_tree_node_view').RulesTreeNodeView

class exports.RulesTreeView extends Backbone.View

  id: 'rules-tree-view'
  tagName: 'ul'

  initialize: ->
    _.bindAll(this, "render")
    @model.bind("change", @render)

  render: ->
    view = new RulesTreeNodeView model: @model.get('root')
    console.log "RulesTreeView.render()"
    @$(@el).html view.render().el
