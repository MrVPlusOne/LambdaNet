// All: 9 JSNice: 7 LambdaNet: 7

import * as nock from 'nock';

function mockRCVariableFetch(
    projectId: string,
    varName: string,
    data,
    token: string = 'thetoken'
): nock.Scope {
    let mock: nock.Scope = nock('https://runtimeconfig.googleapis.com')
        .get(`/v1beta1/projects/${projectId}/configs/firebase/variables/${varName}`);

    if (token) {
        mock = mock.matchHeader('Authorization', `Bearer ${token}`);
    }

    return mock.reply(200, {text: JSON.stringify(data)});
}

function mockMetaVariableWatch(
    projectId: string,
    data,
    token: string,
    updateTime: string
): nock.Scope {
    let mock: nock.Scope = nock('https://runtimeconfig.googleapis.com')
        .post(`/v1beta1/projects/${projectId}/configs/firebase/variables/meta:watch`);

    if (token) {
        mock = mock.matchHeader('Authorization', `Bearer ${token}`);
    }

    return mock.reply(200, {
        updateTime,
        state: 'UPDATED',
        text: JSON.stringify(data),
    });
}

function mockMetaVariableWatchTimeout(projectId: string, delay: number, token?: string): nock.Scope {
    let mock: nock.Scope = nock('https://runtimeconfig.googleapis.com')
        .post(`/v1beta1/projects/${projectId}/configs/firebase/variables/meta:watch`);

    if (token) {
        mock = mock.matchHeader('Authorization', `Bearer ${token}`);
    }

    return mock.delay(delay).reply(502);
}
