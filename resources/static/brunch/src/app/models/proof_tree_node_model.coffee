class exports.ProofTreeNode extends Backbone.Model
  # Available attributes:
  # term :: String
  # mcid :: String
  # childTerms :: BackBone.Collection
  # proofResult :: String, can be either Correct, Incomplete or Invalid

  initialize: =>
    @set { childTerms: new Backbone.Collection()
         , mcid: @cid}

  term: =>
    @get('term')

  proofResult: =>
    @get('proofResult')

  setTerm: (tm) =>
    @set({term: tm})

  childTerms: =>
    @get('childTerms')

  setChildren: (data) =>
    childNo = data.children
    # TODO: grab either rhss or urhss from data, depending on some global setting,
    # and insert it as term in the new nodes
    if childNo > 0
      newChildren = new Array()
      for i in [0..childNo-1]
        newChildren.push(new ProofTreeNode({ term: data.urhss[i]
                                           , treeLvl: @get('treeLvl') + 1
                                           , treeLbl: @get('treeLbl') + "." + i}))
      @childTerms().refresh(newChildren)

  # isValid: =>
  #   mod = @
  #   callback = (data) ->
  #      # TODO: Error message
  #     console.log data
  #     # data[0] : Boolean indicating whether we have a successful parse or not
  #     # data[1] : List of error strings indicating what went wroning during parsing
  #     return data[0] && @childTerms().reduce(((acc, nd) -> nd.isValid() && acc), true)

  #   $.ajax
  #     async: false
  #     type:  'POST'
  #     url:   "/check-syntax/term"
  #     data:  @term()
  #     contentType: 'application/json'
  #     dataType: 'json'
  #     success:  callback

    # return true # TODO: Fix this!
    # str = @term()
    # if !str?
    #   return false

    # # Token -> a word with possibly spaces in front and after
    # # Fun   -> Token ( {Token ,}* Token )?
    # # Term  -> Token ( {Fun ,}* Fun ) )
    # token = "\\s*\\w+\\s*"
    # fun   = token + "(\\((" + token + ",\\s*)*" + token + "\\))?\\s*"
    # regex = new RegExp("\\s*^" + token + "\\((" + fun + ",)*\\s*" + fun + "\\)\\s*\\s*$")
    # valid = regex.test str
    # valid && @childTerms().reduce(((acc, nd) -> nd.isValid() && acc), true)

  setProofResult: (data) =>
    @set({proofResult: data.proofCheckResult})
    @trigger('proof')
    i = 0
    f = (x) ->
      x.setProofResult data.proofCheckChildren[i]
      i++
    @childTerms().each f

  reset: =>
     @childTerms().refresh new Array()
