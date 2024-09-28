# Author: Jaehyun Yeom
#
# Description:
#   Unittesting framework for CoffeeScript

unittest = exports? and @ or @unittest = {}

class unittest.AssertionError extends Error

# Base class of all TestCases.
class unittest.TestCase
  name: 'TestCase'

  setUp: ->

  tearDown: ->

  run: ->
    num_tests = 0
    num_passed = 0
    num_failed = 0
    console.log "===== #{@name} ====="
    for name, func of this
      if name[0...4] == 'test'
        ++num_tests
        console.log "Testing " + name + " ..."
        this.setUp()
        try
          func.call(this)
          console.log "Testing " + name + " ... Passed"
          ++num_passed
        catch error
          console.log "  " + error.message
          console.log "Testing " + name + " ... Failed"
          ++num_failed
        finally
          this.tearDown()
    console.log "Among #{num_tests} tests, #{num_passed} passed and #{num_failed} failed."
    console.log ''

  assertEqual: (expected, actual) ->
    if expected != actual
      error = new unittest.AssertionError
      error.name = 'AssertionError'
      error.message = "Should be #{expected} but found #{actual}."
      error.actual = actual
      error.expected = expected
      error.operator = '=='
      throw error

  assertNotEqual: (notExpected, actual) ->
    if expected == actual
      error = new unittest.AssertionError
      error.name = 'AssertionError'
      error.message = "Shouldn't be #{notExpected} but found #{actual}."
      error.actual = actual
      error.notExpected = notExpected
      error.operator = '!='
      throw error

  assertArrayEqual: (expected, actual) ->
    # TODO(jaeyeom): Find more elegant way to compare arrays
    @assertEqual(expected.toString(), actual.toString())

  assertTrue: (condition) ->
    @assertEqual(true, condition)

  assertFalse: (condition) ->
    @assertEqual(false, condition)
