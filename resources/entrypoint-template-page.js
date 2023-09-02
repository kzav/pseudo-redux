'use strict';

import Store from './stores/Store';
import * as action from './actions/{{component-name}}Action';
import * as reducer from './reducers/{{component-name}}Reducer';
import * as view from './views/{{component-name}}View';

(function () {

    const initialState = {
    };

    const reducers = [
        // アクション/リデューサ/ビューの関連を定義
        {{#action-elements}}
            [ action.{{id-descriptor}}, reducer.{{name}},   null],
        {{/action-elements}}
    ];

    const store = new Store(initialState, reducers);

    window.onload = function () {
        // 操作とアクションをバインド
        {{#bind-elements}}
            view.bind{{bind-name}}(e => store.dispatch(action.{{action-name}}Action(e)));
        {{/bind-elements}}
    };
})();