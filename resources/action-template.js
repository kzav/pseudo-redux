'use strict';

//-----------------------------------------------------------------------------
// アクション タイプID
//-----------------------------------------------------------------------------
{{#action-elements}}
export const {{id-descriptor}} = "{{id-value}}";
{{/action-elements}}

//-----------------------------------------------------------------------------
// アクション クリエイタークリエイター
//-----------------------------------------------------------------------------

{{#action-elements}}
// {{comment}}
export function {{name}}Action({currentTarget}) {
    return {
        type: {{id-descriptor}},
        payload: {{action-value}},
    };
}

{{/action-elements}}
