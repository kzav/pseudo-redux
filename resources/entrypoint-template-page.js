'use strict';

import Store from './stores/Store';
import * as action from './actions/{{component-name}}Action';
import * as reducer from './reducers/{{component-name}}Reducer';
import * as view from './views/{{component-name}}View';

(function () {

    // 初期状態定義
    const initialState = {
        {{#state-elements}}
        {{state-id}}: {{{state-value}}},
        {{/state-elements}}
    };

    const reducers = [
        // アクション/リデューサ/ビューの関連を定義
        {{#action-elements}}
        [ action.{{id-descriptor}}, reducer.{{name}},   null],
        {{/action-elements}}
    ];

    // ストア生成
    const store = new Store(initialState, reducers);

    // 初期設定
    window.onload = function () {
        // 操作とアクションをバインド
        {{#bind-elements}}
        view.bind{{bind-name}}(e => store.dispatch(action.{{action-name}}Action(e)));
        {{/bind-elements}}
    };
})();