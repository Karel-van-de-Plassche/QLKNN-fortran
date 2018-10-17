import json
import f90nml
from IPython import embed

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

    nml = f90nml.namelist.Namelist({target_names[0]: nn_dict})

    return nml


if __name__ == '__main__':
    nml = nn_json_to_namelist('./test_nn.json')
    nml.write('../src/' + list(nml.keys())[0] + '.nml', force=True)
