class RulesListItemView extends Backbone.View

  events:
    'click .check'           : 'toggleDone'
    'dblclick .todo-content' : 'edit'
    'click .todo-destroy'    : 'clear'
    'keypress .todo-input'   : 'updateOnEnter'

  initialize: ->
    console.log "RulesListItemView.initialize"
    # @model.bind('change', @render)
    @model.view = @

  render: =>
    console.log "RulesListItemView.render"
    console.log app.templates.rulesListItem(content: @model.toJSON())
    $(@el).html app.templates.rulesListItem(content: @model.toJSON())
    # Bind event directly to input, cause older browsers doesn't
    # support this event on several types of elements.
    # Originally, this event was only applicable to form elements.
    @$('.todo-input').bind 'blur', @update
    @

  toggleDone: ->
    @model.toggle()

  edit: ->
    @$(@el).addClass "editing"
    $('.todo-input').focus()

  update: =>
    @model.save(content: @$('.todo-input').val())
    @$(@el).removeClass "editing"

  updateOnEnter: (e) ->
    @update() if e.keyCode is $.ui.keyCode.ENTER

  remove: ->
    $(@el).remove()

  clear: ->
    @model.clear()

