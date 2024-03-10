'use strict';

import * as core from '../views/view-core';
import * as util from '../views/view-util';
import { isSmartPhone } from '../util';

//-----------------------------------------------------------------------------
// アクション タイプID
//-----------------------------------------------------------------------------
{{#action-elements}}
export const {{id-descriptor}} = "{{id-value}}";
{{/action-elements}}

//-----------------------------------------------------------------------------
// アクション クリエイタ
//-----------------------------------------------------------------------------

{{#action-elements}}
// {{comment}}
export function {{name}}Action({{action-arg}}) {
    return {
        type: {{id-descriptor}},
        payload: {{action-value}},
    };
}

{{/action-elements}}
