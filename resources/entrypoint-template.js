'use strict';

import * as action from './actions/{{component-name}}Action';
import * as reducer from './reducers/{{component-name}}Reducer';
import * as view from './views/{{component-name}}View';

const reducers = [
    // アクション/リデューサ/ビューの関連を定義
{{#action-elements}}
    [ action.{{id-descriptor}}, reducer.{{name}},   null],
{{/action-elements}}
];

// 初期設定
export function setUp(store) {
    // 操作とアクションをバインド
{{#bind-elements}}
    view.bind{{bind-name}}(e => store.dispatch(action.{{action-name}}Action(e)));
{{/bind-elements}}
}
