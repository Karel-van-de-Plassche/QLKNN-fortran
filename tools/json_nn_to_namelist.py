import json
import f90nml
from IPython import embed
import numpy as np
from collections import OrderedDict

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
def nn_json_to_namelist_dict(path):
    with open(path) as f:
        j = json.load(f)
    nml = nn_dict_to_namelist_dict(j)
    return nml

def nn_dict_to_namelist_dict(nn_dict):
    feature_names = nn_dict.pop('feature_names')
    if len(feature_names) == 9:
        if feature_names != qlknn_9D_feature_names:
            raise ValueError('Feature names have to be {!s} exactly'.format(qlknn_9D_feature_names))
    target_names = nn_dict.pop('target_names')
    if len(target_names) != 1:
        raise ValueError('Only single-output NNs supported')
    name = target_names[0]

    nn_dict.pop('_metadata')
    n_layers = int(len([key for key in nn_dict if key.startswith('layer')])/2)
    nml_dict = OrderedDict()
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

    sizes = {'n_hidden_layers': n_layers - 1,
             'n_hidden_nodes': len(nml_dict['weights_input'][0]),
             'n_inputs': len(nml_dict['weights_input']),
             'n_outputs': len(nml_dict['weights_output'][0]),
             }

    for fb in ['factor', 'bias']:
        for tf, vars in zip(['target', 'feature'],
                           [target_names, feature_names]):
            lst = [nn_dict['prescale_' + fb][var] for var in vars]
            nml_dict[tf + '_prescale_' + fb] = lst

    nml_dict['hidden_activation'] = nn_dict.pop('hidden_activation')
    return name, nml_dict, sizes

def nml_dict_to_namelist(nml_dict, sizes):
    nml = f90nml.namelist.Namelist()
    nml['sizes'] = sizes
    nml['net'] = f90nml.namelist.Namelist(nml_dict)
    return nml

type_map = {
    'float64': 'REAL',
    '<U4': 'CHARACTER',
}
def array_to_string(varname, array, type='REAL'):
    init_str = np.array2string(np.ravel(array, 'F'), separator=', ', threshold=1000000)
    init_str = init_str.replace('[', '(/').replace(']', '/)')
    init_str = init_str.replace('\n', ' &\n        ')
    if len(array.shape) > 1:
        shape_str = str(array.shape)
        shape_str = shape_str.replace('(', '(/').replace(')', '/)')
        init_str = ''.join([' '*8, 'RESHAPE (',init_str, ', ', shape_str, ')'])
        shape_str2 = str(array.shape)
        shape_str2 = shape_str2.replace('(', '').replace(')', '')
    else:
        shape_str2 = str(len(array))
    init_str = type_map[str(array.dtype)] + ', DIMENSION(' + shape_str2 + '), PARAMETER :: ' + varname + ' = &\n' + init_str
    return init_str

def nml_dict_to_source(name, nml_dict, target_dir='../src'):
    init_strings = []
    declare_strings = []
    for key, val in nml_dict.items():
        if isinstance(val, list):
            init_str = array_to_string(key, np.array(val))
            init_strings.append(init_str)
            #declare_strings.append(declare_str)
        else:
            print('Do not know what to do with {!s}'.format(key))
            embed()
            raise Exception

    out_path = os.path.join(target_dir, 'net_' + name.lower() + '.f90')
    print('Writing to', out_path)
    with open(out_path, 'w') as f:
        f.write(module_start.format(name.lower()))
        #f.writelines([el + '\n' for el in declare_strings])
        #f.write('\n')
        f.writelines(['    ' + el + '\n' for el in init_strings])
        f.write(module_end.format(name.lower()))
module_start = \
"""
module net_{0!s}
    use qlknn_types
"""
module_end = \
"""
contains
    function {0!s}()
        type(networktype) :: {0!s}
        {0!s}%weights_input = weights_input
        {0!s}%biases_input = biases_input
        {0!s}%biases_hidden = biases_hidden
        {0!s}%weights_hidden = weights_hidden
        {0!s}%weights_output = weights_output
        {0!s}%biases_output = biases_output

        {0!s}%hidden_activation = hidden_activation

        {0!s}%target_prescale_bias = target_prescale_bias
        {0!s}%target_prescale_factor = target_prescale_factor
        {0!s}%feature_prescale_bias = feature_prescale_bias
        {0!s}%feature_prescale_factor = feature_prescale_factor
    end function {0!s}
end module net_{0!s}
"""

import os
def convert_all(path, target_dir='../src'):
    print(path)
    print(os.listdir(path))
    if os.path.isdir(path):
        for file in os.listdir(path):
            filepath = os.path.join(path, file)
            if os.path.isfile(filepath) and file.endswith('.json'):
                print('Converting', filepath)
                name, nml_dict, sizes = nn_json_to_namelist_dict(filepath)
                nml_dict_to_source(name, nml_dict, target_dir=target_dir)
            else:
                print('Skipping', file, 'not a network json')
    else:
        raise ValueError('Please supply a directory path')


if __name__ == '__main__':
    name, nml_dict, sizes = nn_json_to_namelist_dict('./test_nn.json')
    src_str = nml_dict_to_source(name, nml_dict)
    #nml = nml_dict_to_namelist(nml_dict, sizes)
    #nml.write('../src/' + name + '.nml', force=True)
