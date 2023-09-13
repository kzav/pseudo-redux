'use strict';

import * as actionCreator from '../actions/{{component-name}}Action';

{{#action-elements}}
// {{comment}}
export function {{name}}(action, state, dispatcher) {
    {{{reducer-code}}}
    return state;
}

{{/action-elements}}
