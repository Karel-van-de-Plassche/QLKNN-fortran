import json
import f90nml
from IPython import embed
import numpy as np

qlknn_9D_feature_names = [
        "Zeff",
        "Ati",
        "Ate",
        "An",
        "q",
        "smag",
        "x",
        "Ti_Te",
        "logNustar",
    ]
def nn_json_to_namelist(path):
    with open(path) as f:
        j = json.load(f)
    nml = nn_dict_to_namelist(j)
    return nml

def nn_dict_to_namelist(nn_dict):
    feature_names = nn_dict.pop('feature_names')
    if len(feature_names) == 9:
        if feature_names != qlknn_9D_feature_names:
            raise ValueError('Feature names have to be {!s} exactly'.format(qlknn_9D_feature_names))
    target_names = nn_dict.pop('target_names')
    if len(target_names) != 1:
        raise ValueError('Only single-output NNs supported')

    nn_dict.pop('_metadata')
    n_layers = int(len([key for key in nn_dict if key.startswith('layer')])/2)
    nml_dict = {}
    for layer_type, i_layers in zip(['input', 'hidden', 'output'],
                                   [[1], list(range(2, n_layers)), [n_layers]]):
        for wb in ['weights', 'biases']:
            layerlist = []
            for i_layer in i_layers:
               layerlist.append(nn_dict.pop(''.join(['layer', str(i_layer), '/', wb, '/Variable:0'])))
            if len(layerlist) > 1:
                nml_dict[wb + '_' + layer_type] = np.stack(layerlist, axis=-1).tolist()
            else:
                nml_dict[wb + '_' + layer_type] = layerlist[0]

    for fb in ['factor', 'bias']:
        for tf, var in zip(['target', 'feature'],
                           [target_names, feature_names]):
            pass

    nml_dict['hidden_activation'] = nn_dict.pop('hidden_activation')
    nml = f90nml.namelist.Namelist({target_names[0]: nml_dict})

    return nml


if __name__ == '__main__':
    nml = nn_json_to_namelist('./test_nn.json')
    nml.write('../src/' + list(nml.keys())[0] + '.nml', force=True)
